package views.support

import com.gu.membership.model.{BillingPeriod, Current, PaidTierPlan, Price}
import com.gu.membership.salesforce.PaidTier
import com.gu.membership.stripe.Stripe.Card
import com.gu.membership.zuora.rest
import com.gu.membership.zuora.soap.models.Queries.PreviewInvoiceItem
import model.MembershipCatalog
import org.joda.time.{DateTime, LocalDate}

case class CurrentSummary(tier: PaidTier, startDate: LocalDate, payment: Price, card: Card)

case class TargetSummary(tier: PaidTier, firstPayment: Price, nextPayment: Price, nextPaymentDate: LocalDate)

case class UpgradeSummary(billingPeriod: BillingPeriod, current: CurrentSummary, target: TargetSummary) {
  lazy val transactionDate: LocalDate = DateTime.now.toLocalDate
}

object UpgradeSummary {
  case class UpgradeSummaryError(msg: String)(implicit restSub: rest.Subscription, targetTier: PaidTier) extends Throwable {
    override def getMessage = s"Failure while trying to display an upgrade summary for the subscription ${restSub.subscriptionNumber} to $targetTier: $msg"
  }

  def apply(catalog: MembershipCatalog, invoices: Seq[PreviewInvoiceItem], restSub: rest.Subscription, targetTier: PaidTier, card: Card): UpgradeSummary = {
    val currentRatePlan = restSub.currentRatePlanUnsafe()
    val currentCharge = currentRatePlan.currentChargeUnsafe()
    val currentTierPlan = catalog.unsafePaidTierPlan(currentRatePlan.productRatePlanId)
    val currentPrice = currentCharge.unsafePrice

    implicit val s = restSub
    implicit val t = targetTier

    val sortedInvoiceItems = invoices.sortBy(_.price)
    val futureSubscriptionInvoice = sortedInvoiceItems.lastOption.getOrElse(
      throw UpgradeSummaryError("Expected at least one invoice item in the preview")
    )
    val firstPayment = Price(invoices.map(_.price).sum.toInt, currentPrice.currency)

    val targetTierPlan = PaidTierPlan(targetTier, currentTierPlan.billingPeriod, Current)
    val targetTierPlanDetails = catalog.paidTierPlanDetails(targetTierPlan)
    val targetPrice = targetTierPlanDetails.pricingByCurrency.getPrice(currentPrice.currency).getOrElse(
      throw UpgradeSummaryError(s"Could not find a price for currency ${currentPrice.currency} for rate plan $targetTierPlan")
    )

    val currentSummary =
      CurrentSummary(
        tier = currentTierPlan.tier,
        startDate = restSub.contractEffectiveDate.toLocalDate,
        payment = currentCharge.unsafePrice,
        card = card
      )

    val targetSummary =
      TargetSummary(
        tier = targetTier,
        firstPayment =  firstPayment,
        nextPayment = targetPrice,
        nextPaymentDate = futureSubscriptionInvoice.serviceStartDate.toLocalDate
      )

    UpgradeSummary(currentTierPlan.billingPeriod, currentSummary, targetSummary)
  }
}
