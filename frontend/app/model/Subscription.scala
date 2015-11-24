package model

import com.gu.i18n.Currency
import com.gu.membership.model.{FreeTierPlan, PaidTierPlan, Price, TierPlan}
import com.gu.membership.salesforce.ContactId
import com.gu.membership.zuora.rest
import com.gu.membership.zuora.soap.models.Queries.Account
import org.joda.time.LocalDate


trait Subscription {
  def number: String
  def accountCurrency: Currency
  def contactId: ContactId
  def features: Set[FeatureChoice]
  def plan: TierPlan
  def startDate: LocalDate
}

case class FreeSubscription(accountCurrency: Currency,
                            number: String,
                            contactId: ContactId,
                            plan: FreeTierPlan,
                            startDate: LocalDate) extends Subscription {
  val features = Set.empty[FeatureChoice]
}

object FreeSubscription {
  def apply(contactId: ContactId,
            plan: FreeTierPlan,
            account: Account,
            sub: rest.Subscription): FreeSubscription = {

     FreeSubscription(
       accountCurrency = Subscription.accountCurrency(account),
       number = sub.subscriptionNumber,
       contactId = contactId,
       plan = plan,
       startDate = sub.contractEffectiveDate.toLocalDate)

  }
}

case class PaidSubscription(accountCurrency: Currency,
                            contactId: ContactId,
                            number: String,
                            defaultPaymentMethod: Option[String],
                            features: Set[FeatureChoice],
                            plan: PaidTierPlan,
                            price: Price,
                            startDate: LocalDate,
                            firstPaymentDate: LocalDate,
                            chargeThroughDate: Option[LocalDate]) extends Subscription


object PaidSubscription {
  def apply(contactId: ContactId,
            plan: PaidTierPlan,
            account: Account,
            sub: rest.Subscription): PaidSubscription = {

    val rp = sub.currentRatePlanUnsafe()
    PaidSubscription(accountCurrency = Subscription.accountCurrency(account),
                     contactId = contactId,
                     number = sub.subscriptionNumber,
                     defaultPaymentMethod = account.defaultPaymentMethodId,
                     features = rp.subscriptionProductFeatures.map(f => FeatureChoice.byId(f.featureCode)).toSet,
                     plan = plan,
                     price = rp.currentChargeUnsafe().unsafePrice,
                     startDate = sub.contractEffectiveDate.toLocalDate,
                     firstPaymentDate = sub.customerAcceptanceDate.toLocalDate,
                     chargeThroughDate = rp.currentCharge().flatMap(_.chargedThroughDate).map(_.toLocalDate))
  }
}

object Subscription {
  def accountCurrency(account: Account): Currency = Currency.fromString(account.currency).getOrElse {
    throw new IllegalArgumentException(s"Cannot parse currency ${account.currency} for account ${account.id}")
  }

  def apply(catalog: MembershipCatalog)(contactId: ContactId,
                                        account: Account,
                                        sub: rest.Subscription): Subscription =

    catalog.unsafeTierPlan(sub.currentRatePlanUnsafe().productRatePlanId) match {
      case free: FreeTierPlan => FreeSubscription(contactId, free, account, sub)
      case paid: PaidTierPlan => PaidSubscription(contactId, paid, account, sub)
    }
}
