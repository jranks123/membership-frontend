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

case class PaidToPaidUpgradeSummary(billingPeriod: BillingPeriod, current: CurrentSummary, target: TargetSummary) {
  lazy val transactionDate: LocalDate = DateTime.now.toLocalDate
}

object PaidToPaidUpgradeSummary {
  case class UpgradeSummaryError(subNumber: String, targetTier: PaidTier)(msg: String) extends Throwable {
    override def getMessage = s"Failure while trying to display an upgrade summary for the subscription ${subNumber} to $targetTier: $msg"
  }

  def apply(catalog: MembershipCatalog, invoices: Seq[PreviewInvoiceItem], sub: model.PaidSubscription, targetTier: PaidTier, card: Card): PaidToPaidUpgradeSummary = {
    val upgradeError = UpgradeSummaryError(sub.number, targetTier) _
    val accountCurrency = sub.accountCurrency

    val sortedInvoiceItems = invoices.sortBy(_.price)
    val futureSubscriptionInvoice = sortedInvoiceItems.lastOption.getOrElse(
      throw upgradeError("Expected at least one invoice item in the preview")
    )
    val firstPayment = Price(invoices.map(_.price).sum.toInt, accountCurrency)

    val billingPeriod = sub.plan.billingPeriod
    val targetTierPlan = PaidTierPlan(targetTier, billingPeriod, Current)

    val targetTierPlanDetails = catalog.paidTierPlanDetails(targetTierPlan)
    val targetPrice = targetTierPlanDetails.pricingByCurrency.getPrice(accountCurrency).getOrElse(
      throw upgradeError(s"Could not find a price for currency $accountCurrency for rate plan $targetTierPlan")
    )

    val currentSummary =
      CurrentSummary(
        tier = sub.plan.tier,
        startDate = sub.startDate,
        payment = sub.recurringPrice,
        card = card
      )

    val targetSummary =
      TargetSummary(
        tier = targetTier,
        firstPayment =  firstPayment,
        nextPayment = targetPrice,
        nextPaymentDate = futureSubscriptionInvoice.serviceStartDate.toLocalDate
      )

    PaidToPaidUpgradeSummary(billingPeriod, currentSummary, targetSummary)
  }
}
