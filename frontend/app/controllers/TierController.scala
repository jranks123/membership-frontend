package controllers

import com.gu.i18n.{CountryGroup, GBP}
import com.gu.identity.play.PrivateFields
import com.gu.memsub.BillingPeriod
import com.gu.memsub.BillingPeriod._
import com.gu.salesforce._
import com.gu.stripe.Stripe
import com.gu.stripe.Stripe.Serializer._
import com.gu.zuora.soap.models.errors.ResultError
import forms.MemberForm._
import model._
import org.joda.time.LocalDate
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.mvc.{Controller, Result}
import play.filters.csrf.CSRF.Token.getToken
import services._
import tracking.ActivityTracking
import utils.TierChangeCookies
import views.support.PageInfo.CheckoutForm
import views.support.{CountryWithCurrency, PageInfo, PaidToPaidUpgradeSummary}

import scala.concurrent.Future
import scala.language.implicitConversions

trait DowngradeTier extends ActivityTracking with CatalogProvider
                                             with SubscriptionServiceProvider
                                             with MemberServiceProvider {
  self: TierController =>

  def downgradeToFriend() = PaidMemberAction.async { implicit request =>
    for {
      subs <- subscriptionService.unsafeGetPaid(request.member)
    } yield {
      Ok(views.html.tier.downgrade.confirm(catalog.unsafeFindPaid(subs.productRatePlanId).tier, catalog))
    }
  }

  def downgradeToFriendConfirm = PaidMemberAction.async { implicit request => // POST
    for {
      cancelledSubscription <- memberService.downgradeSubscription(request.member, request.user)
    } yield Redirect(routes.TierController.downgradeToFriendSummary)
  }

  def downgradeToFriendSummary = PaidMemberAction.async { implicit request =>
    for {
      // The downgrade is effective at the end of the charge date, so the current tier is still paid
      subscription <- subscriptionService.unsafeGetPaid(request.member)
    } yield {
      val startDate = subscription.chargedThroughDate.map(_.plusDays(1)).getOrElse(LocalDate.now).toDateTimeAtCurrentTime()
      Ok(views.html.tier.downgrade.summary(subscription, catalog, startDate))
        .discardingCookies(TierChangeCookies.deletionCookies:_*)
    }
  }
}

trait UpgradeTier extends StripeServiceProvider {
  self: TierController =>

  def upgrade(target: PaidTier) = ChangeToPaidAction(target).async { implicit request =>
    val sub = request.subscription
    val stripeKey = Some(stripeService.publicKey)
    val currency = sub.currency
    val countriesWithCurrency = CountryWithCurrency.withCurrency(currency)
    val targetPlans = catalog.paidPlans(target)

    val identityUserFieldsF =
      IdentityService(IdentityApi)
        .getIdentityUserView(request.user, IdentityRequest(request))
        .map(_.privateFields)

    // Preselect the country from Identity fields
    // but the currency from Zuora account
    def pageInfo(pf: PrivateFields, billingPeriod: BillingPeriod): PageInfo = {
      val selectedCountry = pf.billingCountry.orElse(pf.country).flatMap { name =>
        CountryGroup.countries.find(_.name == name)
      }
      val formI18n = CheckoutForm(selectedCountry, currency, billingPeriod)
      PageInfo(initialCheckoutForm = formI18n, stripePublicKey = stripeKey)
    }

    def fromFree(subscription: FreeSubscription, contact: Contact[FreeTierMember, NoPayment]): Future[Result] =
      for {
        privateFields <- identityUserFieldsF
      } yield {
        val currentPlan = catalog.freePlan(contact.tier)
        Ok(views.html.tier.upgrade.freeToPaid(
          currentPlan,
          targetPlans.year,
          countriesWithCurrency,
          privateFields,
          pageInfo(privateFields, year)
        )(getToken, request))
      }

    def fromPaid(subscription: PaidSubscription, contact: Contact[PaidTierMember, StripePayment]): Future[Result] = {
      val stripeCustomerF = stripeService.Customer.read(contact.stripeCustomerId)
      val targetPlanId = targetPlans.get(subscription.plan.billingPeriod).productRatePlanId

      for {
        previewItems <- memberService.previewUpgradeSubscription(
          subscription, targetPlanId)
        customer <- stripeCustomerF
        privateFields <- identityUserFieldsF
      } yield {
        val summary = PaidToPaidUpgradeSummary(previewItems, subscription, targetPlanId, customer.card)
        val flashMsgOpt = request.flash.get("error").map(FlashMessage.error)

        Ok(views.html.tier.upgrade.paidToPaid(
          summary,
          privateFields,
          pageInfo(privateFields, subscription.plan.billingPeriod),
          flashMsgOpt
        )(getToken, request))
      }
    }

    (request.subscription, request.member) match {
      case (sub: FreeSubscription, Contact(d, t: FreeTierMember, p: NoPayment)) =>
        fromFree(sub, Contact(d, t, p))
      case (sub: PaidSubscription, Contact(d, t: PaidTierMember, p: StripePayment)) =>
        fromPaid(sub, Contact(d, t, p))
      case _ =>
        Future {
          val msg = s"Zuora account ${sub.accountId} is inconsistent with its corresponding Salesforce information"
          throw new IllegalStateException(msg)
        }
    }
  }

  def upgradeConfirm(target: PaidTier) = ChangeToPaidAction(target).async { implicit request =>
    val identityRequest = IdentityRequest(request)

    def handleFree(freeMember: FreeSFMember)(form: FreeMemberChangeForm) = for {
      memberId <- memberService.upgradeFreeSubscription(freeMember, form, identityRequest)
    } yield Ok(Json.obj("redirect" -> routes.TierController.upgradeThankyou(target).url))

    def handlePaid(paidMember: PaidSFMember)(form: PaidMemberChangeForm) = {
      val reauthFailedMessage: Future[Result] = Future {
        Redirect(routes.TierController.upgrade(target))
          .flashing("error" ->
          s"That password does not match our records. Please try again.")
      }

      def doUpgrade(): Future[Result] = {
        memberService.upgradePaidSubscription(paidMember, form, identityRequest).map {
          _ => Redirect(routes.TierController.upgradeThankyou(target))
        }
      }

      for {
        status <- IdentityService(IdentityApi).reauthUser(paidMember.email, form.password, identityRequest)
        result <- if (status == 200) doUpgrade() else reauthFailedMessage
      } yield result

    }

    val futureResult = request.member match {
      case Contact(d, m: FreeTierMember, p: NoPayment) => freeMemberChangeForm.bindFromRequest.fold(redirectToUnsupportedBrowserInfo, handleFree(Contact(d, m, p)))
      case Contact(d, m: PaidTierMember, p: StripePayment) => paidMemberChangeForm.bindFromRequest.fold(redirectToUnsupportedBrowserInfo, handlePaid(Contact(d, m, p)))
      case Contact(d, m, p) => throw new IllegalStateException(s"Inconsistent Salesforce state, contact with id ${d.salesforceContactId} is a $m but has a $p payment method")
    }

    futureResult.map(_.discardingCookies(TierChangeCookies.deletionCookies:_*)).recover {
      case error: Stripe.Error => Forbidden(Json.toJson(error))
      case error: ResultError => Forbidden
      case error: ScalaforceError => Forbidden
    }
  }

  def upgradeThankyou(tier: PaidTier) = Joiner.thankyou(tier, upgrade=true)
}

trait CancelTier extends CatalogProvider {
  self: TierController =>

  def cancelTier() = MemberAction { implicit request =>
    Ok(views.html.tier.cancel.confirm(request.member.tier, catalog))
  }

  def cancelTierConfirm() = MemberAction.async { implicit request =>
    memberService.cancelSubscription(request.member, request.user).map { _ =>
      request.member.tier match {
        case m: FreeTierMember => Redirect(routes.TierController.cancelFreeTierSummary())
        case _ => Redirect(routes.TierController.cancelPaidTierSummary())
      }
    }
  }

  def cancelFreeTierSummary = AuthenticatedAction(
    Ok(views.html.tier.cancel.summaryFree())
  )

  def cancelPaidTierSummary = PaidMemberAction.async { implicit request =>
    subscriptionService.unsafeGetPaid(request.member).map { sub =>
      Ok(views.html.tier.cancel.summaryPaid(sub))
        .discardingCookies(TierChangeCookies.deletionCookies:_*)
    }
  }
}

trait TierController extends Controller with UpgradeTier with DowngradeTier with CancelTier {
  def change() = MemberAction { implicit request =>
    implicit val currency = GBP
    Ok(views.html.tier.change(request.member.tier, catalog))
  }
}

object TierController extends TierController
