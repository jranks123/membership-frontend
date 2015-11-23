package services

import com.github.nscala_time.time.Imports._
import com.gu.i18n.Currency
import com.gu.membership.model._
import com.gu.membership.salesforce.ContactId
import com.gu.membership.salesforce.Tier._
import com.gu.membership.stripe.Stripe
import com.gu.membership.touchpoint.TouchpointBackendConfig.BackendType
import com.gu.membership.util.FutureSupplier
import com.gu.membership.zuora.soap.Readers._
import com.gu.membership.zuora.soap._
import com.gu.membership.zuora.soap.actions.Actions._
import com.gu.membership.zuora.soap.actions.subscribe
import com.gu.membership.zuora.soap.actions.subscribe.Subscribe
import com.gu.membership.zuora.soap.models.Queries._
import com.gu.membership.zuora.soap.models.Results._
import com.gu.membership.zuora.soap.models._
import com.gu.membership.zuora.{rest, soap}
import com.gu.monitoring.ServiceMetrics
import com.typesafe.scalalogging.LazyLogging
import configuration.RatePlanIds
import forms.MemberForm.JoinForm
import model._
import org.joda.time.DateTime

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

case class SubscriptionServiceError(s: String) extends Throwable {
  override def getMessage: String = s
}

object SubscriptionService {
  val membershipProductType = "Membership"
  val productRatePlanChargeModel = "FlatFee"

  /**
   * A Zuora subscription may have many versions as it is amended, some of which can be in the future (ie. downgrading
   * from a paid tier - because we don't refund that user, the downgrade is instead set to the point in the future when
   * their paid period ends).
   *
   * The Zuora API does not explicitly tell you what the *current* subscription version is. You have to work it out,
   * by looking at the 'amendments', finding the first amendment that has yet occurred. That amendment will give you the
   * id of the subscription it modified - and THAT will be the *current* subscription version.
   */
  def findCurrentSubscriptionStatus(subscriptionVersions: Seq[Subscription], amendments: Seq[Amendment]): SubscriptionStatus = {
    val firstAmendmentWhichHasNotYetOccurredOpt = // this amendment *will have modified the current subscription*
      sortAmendments(subscriptionVersions, amendments).find(_.contractEffectiveDate.isAfterNow)

    val latestSubVersion = subscriptionVersions.maxBy(_.version)

    firstAmendmentWhichHasNotYetOccurredOpt.fold(SubscriptionStatus(latestSubVersion, None, None)) { amendmentOfCurrentSub =>
      val currentSubId = amendmentOfCurrentSub.subscriptionId
      val currentSubVersion = subscriptionVersions.find(_.id == currentSubId).get
      SubscriptionStatus(currentSubVersion, Some(latestSubVersion), Some(amendmentOfCurrentSub.amendType))
    }
  }

  /**
   * Given an array of subscription invoice items, return only items corresponding to the last invoice
   * @param items The incoming list of items with many invoices, potentially associated to multiple subscription versions
   */
  def latestInvoiceItems(items: Seq[InvoiceItem]): Seq[InvoiceItem] = {
    if(items.isEmpty)
      items
    else {
      val sortedItems = items.sortBy(_.serviceStartDate)
      sortedItems.filter(_.subscriptionId == sortedItems.last.subscriptionId)
    }
  }

  /**
   * @param subscriptions
   * @param amendments which are returned by the Zurora API in an unpredictable order
   * @return amendments which are sorted by the subscription version number they point to (the sub they amended)
   */
  def sortAmendments(subscriptions: Seq[Subscription], amendments: Seq[Amendment]) = {
    val versionsNumberBySubVersionId = subscriptions.map { sub => (sub.id, sub.version) }.toMap
    amendments.sortBy { amendment => versionsNumberBySubVersionId(amendment.subscriptionId) }
  }

  def sortInvoiceItems(items: Seq[InvoiceItem]) = items.sortBy(_.chargeNumber)

  def sortPreviewInvoiceItems(items: Seq[PreviewInvoiceItem]) = items.sortBy(_.price)

  def sortSubscriptions(subscriptions: Seq[Subscription]) = subscriptions.sortBy(_.version)

  def featuresPerTier(zuoraFeatures: Seq[Feature])(plan: TierPlan, choice: Set[FeatureChoice]): Seq[Feature] = {
    def byChoice(choice: Set[FeatureChoice]) =
      zuoraFeatures.filter(f => choice.map(_.zuoraCode).contains(f.code))

    plan.tier match {
      case Patron => byChoice(FeatureChoice.all)
      case Partner => byChoice(choice).take(1)
      case _ => Nil
    }
  }
}

class SubscriptionService(val zuoraSoapClient: soap.ClientWithFeatureSupplier,
			                    val zuoraRestClient: rest.Client,
			                    val metrics: ServiceMetrics,
			                    val ratePlanIds: RatePlanIds,
			                    val bt: BackendType) extends LazyLogging {

  import SubscriptionService._

  implicit private val _bt = bt

  val membershipCatalog: FutureSupplier[MembershipCatalog] = new FutureSupplier[MembershipCatalog](
    productRatePlans.map(MembershipCatalog.unsafeFromZuora(ratePlanIds))
  )

  def productRatePlans: Future[Seq[rest.ProductRatePlan]] =
     zuoraRestClient.productCatalog.map(_.products.flatMap(_.productRatePlans))

  def getMembershipCatalog(): Future[MembershipCatalog.Val[MembershipCatalog]] =
    productRatePlans.map(MembershipCatalog.fromZuora(ratePlanIds))

  private def subscriptionVersions(subscriptionNumber: String): Future[Seq[Subscription]] = for {
    subscriptions <- zuoraSoapClient.query[Subscription](SimpleFilter("Name", subscriptionNumber))
  } yield subscriptions

  def account(contact: ContactId): Future[Account] =
    accountWithLatestMembershipSubscription(contact).map(_._1)

  private def accounts(contact: ContactId): Future[Seq[Account]] =
    zuoraSoapClient.query[Account](SimpleFilter("crmId", contact.salesforceAccountId))

  def accountWithLatestMembershipSubscription(contact: ContactId): Future[(Account, rest.Subscription)] = for {
    accounts <- accounts(contact)
    accountAndSubscriptionOpts <- Future.traverse(accounts) { account =>
      zuoraRestClient.latestSubscriptionOpt(ratePlanIds.ids, Set(account.id)).map(account -> _)
    }
  } yield {
      accountAndSubscriptionOpts.collect { case (account, Some(subscription)) =>
        account -> subscription
      }.sortBy(_._2.termStartDate).lastOption.getOrElse(throw new SubscriptionServiceError(
        s"Cannot find a membership subscription for account ids ${accounts.map(_.id)}"))
    }

  def memberTierFeatures(contactId: ContactId): Future[Seq[rest.Feature]] =
    for {
      (_, subscription) <- accountWithLatestMembershipSubscription(contactId)
    } yield subscription
      .latestWhiteListedRatePlan(ratePlanIds.ids).toSeq
      .flatMap(_.subscriptionProductFeatures)

  private def memberTierFeatures(subscription: rest.Subscription): Seq[rest.Feature] =
    subscription.currentRatePlan().toSeq.flatMap(_.subscriptionProductFeatures)

  /**
   * @return the current and the future subscription version of the user if
   *         they have a pending amendment (Currently this is the case only of downgrades, as upgrades
   *         are effective immediately)
   */
  def getSubscriptionStatus(memberId: ContactId): Future[SubscriptionStatus] =
    accountWithLatestMembershipSubscription(memberId).flatMap(accountWithSub =>
      getSubscriptionStatus(accountWithSub._2))

  def getSubscriptionStatus(subscription: rest.Subscription): Future[SubscriptionStatus] = for {
    subscriptionVersions <- subscriptionVersions(subscription.subscriptionNumber)
    amendments <- zuoraSoapClient.query[Amendment](OrFilter(subscriptionVersions.map(s => ("SubscriptionId", s.id)): _*))
  } yield findCurrentSubscriptionStatus(subscriptionVersions, amendments)

  def getCurrentSubscriptionDetails(memberId: ContactId): Future[SubscriptionDetails] =
    accountWithLatestMembershipSubscription(memberId).map { case (_, sub)=> SubscriptionDetails(sub) }

  /*
   * If the member is entitled to complimentary tickets return its Zuora account's corresponding usage records count.
   * Returns none otherwise
   */
  def getUsageCountWithinTerm(subscription: rest.Subscription, unitOfMeasure: String): Future[Option[Int]] = {
    val features = memberTierFeatures(subscription)
    val startDate = DateTimeHelpers.formatDateTime(subscription.termStartDate)

    val usageCountF = zuoraSoapClient.query[Usage](AndFilter(("StartDateTime", startDate),
							                                               ("SubscriptionNumber", subscription.subscriptionNumber),
							                                               ("UOM", unitOfMeasure))).map(_.size)
    for {
      usageCount <- usageCountF
    } yield {
      val hasComplimentaryTickets = features.exists(_.featureCode == FreeEventTickets.zuoraCode)
      if (!hasComplimentaryTickets) None else Some(usageCount)
    }
  }

  def createPaymentMethod(memberId: ContactId, customer: Stripe.Customer): Future[UpdateResult] = for {
    (account, _) <- accountWithLatestMembershipSubscription(memberId)
    paymentMethod <- zuoraSoapClient.authenticatedRequest(CreatePaymentMethod(account, customer))
    result <- zuoraSoapClient.authenticatedRequest(EnablePayment(account, paymentMethod))
  } yield result

  def createSubscription(memberId: ContactId,
                         joinData: JoinForm,
                         customerOpt: Option[Stripe.Customer],
                         paymentDelay: Option[Period],
                         casId: Option[String]): Future[SubscribeResult] = for {
    zuoraFeatures <- zuoraSoapClient.featuresSupplier.get()
    ratePlanId <- findRatePlanId(joinData.plan)
    result <- zuoraSoapClient.authenticatedRequest(Subscribe(account = subscribe.Account.stripe(memberId, customerOpt.isDefined),
                                                             paymentMethodOpt = customerOpt.map(subscribe.CreditCardReferenceTransaction),
                                                             ratePlanId = ratePlanId,
                                                             firstName=joinData.name.first,
                                                             lastName=joinData.name.last,
                                                             address=joinData.deliveryAddress,
                                                             casIdOpt = casId,
                                                             paymentDelay = paymentDelay,
                                                             ipAddressOpt = None,
                                                             featureIds = featuresPerTier(zuoraFeatures)(joinData.plan, joinData.featureChoice).map(_.id)))
  } yield result

  def getPaymentSummary(memberId: ContactId): Future[PaymentSummary] = {
    for {
      (_, subscription) <- accountWithLatestMembershipSubscription(memberId)
      subStatus <- getSubscriptionStatus(subscription)
      invoiceItems <- zuoraSoapClient.query[InvoiceItem](SimpleFilter("SubscriptionNumber", subStatus.currentVersion.name))
      filteredInvoices = latestInvoiceItems(invoiceItems)
    } yield {
      val currency = subscription.currentRatePlanUnsafe().currentChargeUnsafe().priceWithCurrencyUnsafe._1
      PaymentSummary(filteredInvoices, currency)
    }
  }

  def getMembershipSubscriptionSummary(memberId: ContactId): Future[MembershipSummary] = {
    val latestSubF = accountWithLatestMembershipSubscription(memberId).map(_._2)

    def hasUserBeenInvoiced(memberId: ContactId) = latestSubF.map(_.customerAcceptanceDate.isBeforeNow)

    def getSummaryViaSubscriptionAmend(memberId: ContactId) = for {
      latestSubscription <- latestSubF
      result <- zuoraSoapClient.authenticatedRequest(SubscriptionDetailsViaAmend(latestSubscription.id, latestSubscription.customerAcceptanceDate))
    } yield {
      val subscriptionDetails = SubscriptionDetails(latestSubscription)
      assert(result.invoiceItems.nonEmpty, "Subscription with delayed payment returning zero invoice items in SubscriptionDetailsViaAmend call")
      val firstPreviewInvoice = result.invoiceItems.sortBy(_.serviceStartDate).head

      MembershipSummary(
        startDate = latestSubscription.termStartDate,
        firstPaymentEndDate = firstPreviewInvoice.serviceEndDate,
        currency = subscriptionDetails.currency,
        amountPaidToday = None,
        planAmount = subscriptionDetails.planAmount,
        nextPaymentPrice = firstPreviewInvoice.price,
        nextPaymentDate = firstPreviewInvoice.serviceStartDate,
        renewalDate = firstPreviewInvoice.renewalDate
      )
    }

    def getSummaryViaInvoice(memberId: ContactId) =
      getPaymentSummary(memberId).map { payment =>
        MembershipSummary(
          startDate = payment.current.serviceStartDate,
          firstPaymentEndDate = payment.current.serviceEndDate,
          currency = payment.currency,
          amountPaidToday = Some(payment.totalPrice),
          planAmount = payment.current.price,
          nextPaymentPrice = payment.current.price,
          nextPaymentDate = payment.current.nextPaymentDate,
          renewalDate = payment.current.nextPaymentDate
        )
      }

    for {
      userInvoiced <- hasUserBeenInvoiced(memberId)
      summary <- if (userInvoiced) getSummaryViaInvoice(memberId) else getSummaryViaSubscriptionAmend(memberId)
    } yield summary
  }

  def cancelSubscription(contactId: ContactId, instant: Boolean): Future[AmendResult] =
    for {
      restSub <- subWithNoPendingAmend(contactId)
      plan = restSub.currentRatePlanUnsafe()
      cancelDate = if (instant) DateTime.now else plan.currentChargeUnsafe().chargedThroughDate
      result <- zuoraSoapClient.authenticatedRequest(CancelPlan(restSub.id, plan.id, cancelDate))
    } yield result

  def downgradeSubscription(contactId: ContactId, newTierPlan: TierPlan): Future[AmendResult] = {
    //if the member has paid upfront so they should have the higher tier until charged date has completed then be downgraded
    //otherwise use customer acceptance date (which should be in the future)
    def effectiveFrom(sub: rest.Subscription) =
      sub.currentRatePlanUnsafe().currentChargeUnsafe().chargedThroughDate

    for {
      restSub <- subWithNoPendingAmend(contactId)
      ratePlanId <- findRatePlanId(newTierPlan)
      result <- zuoraSoapClient.authenticatedRequest(DowngradePlan(
        restSub.id,
        restSub.currentRatePlanUnsafe().id,
        ratePlanId,
        effectiveFrom(restSub)))
    } yield result
  }

  def upgradeSubscription(memberId: ContactId, newTierPlan: TierPlan, preview: Boolean, featureChoice: Set[FeatureChoice]): Future[AmendResult] = {
    import SubscriptionService._
    for {
      restSub <- subWithNoPendingAmend(memberId)
      zuoraFeatures <- zuoraSoapClient.featuresSupplier.get()
      newRatePlanId <- findRatePlanId(newTierPlan)
      choice = featuresPerTier(zuoraFeatures)(newTierPlan, featureChoice).map(_.id)
      result <- zuoraSoapClient.authenticatedRequest(
        UpgradePlan(restSub.id, restSub.currentRatePlanUnsafe().id, newRatePlanId, preview, choice))
    } yield result
  }

  private def findRatePlanId(newTierPlan: TierPlan): Future[String] = {
    membershipCatalog.get().map(_.ratePlanId(newTierPlan))
  }

  private def subWithNoPendingAmend(contactId: ContactId): Future[rest.Subscription] =
    for {
      (_, restSub) <- accountWithLatestMembershipSubscription(contactId)
      status <- getSubscriptionStatus(restSub)
    } yield {
      if (status.futureVersionIdOpt.isEmpty) {
       restSub
      } else throw SubscriptionServiceError("Cannot amend subscription, amendments are already pending")
    }

  def getSubscriptionsByCasId(casId: String): Future[Seq[Subscription]] =
    zuoraSoapClient.query[Subscription](SimpleFilter("CASSubscriberID__c", casId))

  def getCurrency(contactId: ContactId): Future[Currency] =
    account(contactId).map { acc =>
      Currency.fromString(acc.currency).getOrElse(
        throw SubscriptionServiceError(s"Could not parse currency '${acc.currency}' for account ${acc.id}")
      )
    }
}
