package controllers

import com.gu.i18n.GBP
import com.gu.identity.play.PrivateFields
import com.gu.membership.salesforce._
import com.gu.membership.stripe.Stripe
import com.gu.membership.stripe.Stripe.Serializer._
import com.gu.membership.zuora.soap.models.errors.ResultError
import forms.MemberForm._
import model.{FlashMessage, FreeSubscription, PageInfo, PaidSubscription}
import org.joda.time.{LocalDate, DateTime}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.mvc.{Controller, DiscardingCookie, Result}
import play.filters.csrf.CSRF.Token.getToken
import services._
import tracking.ActivityTracking
import utils.CampaignCode.extractCampaignCode
import views.support.PaidToPaidUpgradeSummary

import scala.concurrent.Future

trait DowngradeTier extends ActivityTracking {
  self: TierController =>

  def downgradeToFriend() = PaidMemberAction.async { implicit request =>
    for {
      cat <- request.catalog
      subs <- request.touchpointBackend.subscriptionService.currentSubscription(request.member)
    } yield {
      Ok(views.html.tier.downgrade.confirm(cat.unsafePaidTierPlan(subs.productRatePlanId).tier, cat))
    }
  }

  def downgradeToFriendConfirm = PaidMemberAction.async { implicit request => // POST
    for {
      cancelledSubscription <- request.touchpointBackend.downgradeSubscription(request.member, request.user, extractCampaignCode(request))
    } yield Redirect(routes.TierController.downgradeToFriendSummary)
  }

  def downgradeToFriendSummary = PaidMemberAction.async { implicit request =>
    val subscriptionService = request.touchpointBackend.subscriptionService
    val catalogF = request.catalog
    val currentTier = request.member.tier
    for {
      subscription <- subscriptionService.currentPaidSubscription(request.member)
      cat <- catalogF
    } yield {
      val startDate = subscription.chargedThroughDate.map(_.plusDays(1)).getOrElse(LocalDate.now).toDateTimeAtCurrentTime()
      Ok(views.html.tier.downgrade.summary(subscription, currentTier, cat, startDate))
    }
  }
}

trait UpgradeTier {
  self: TierController =>

  def upgrade(target: PaidTier) = ChangeToPaidAction(target).async { implicit request =>
    val tp = request.touchpointBackend
    implicit val currency = GBP

    val identityUserFieldsF =
      IdentityService(IdentityApi)
        .getFullUserDetails(request.user, IdentityRequest(request))
        .map(_.privateFields.getOrElse(PrivateFields()))

    val catalog = request.catalog
    val pageInfo = PageInfo.default.copy(stripePublicKey = Some(tp.stripeService.publicKey))

    def previewFreeUpgrade(subscription: model.FreeSubscription, contact: Contact[FreeTierMember, NoPayment]): Future[Result] =
      for {
        privateFields <- identityUserFieldsF
        cat <- catalog
      } yield {
        val currentDetails = cat.freeTierDetails(contact.tier)
        val targetDetails = cat.paidTierDetails(target)
        Ok(views.html.tier.upgrade.freeToPaid(currentDetails, targetDetails, privateFields, pageInfo)(getToken, request.request, currency))
      }

    def previewPaidUpgrade(subscription: model.PaidSubscription, contact: Contact[PaidTierMember, StripePayment]): Future[Result] = {
      val stripeCustomerF = tp.stripeService.Customer.read(contact.stripeCustomerId)

      for {
        previewItems <- MemberService.previewUpgradeSubscription(subscription, contact, target, tp)
        cat <- catalog
        customer <- stripeCustomerF
        privateFields <- identityUserFieldsF
      } yield {
        val summary = PaidToPaidUpgradeSummary(cat, previewItems, subscription, target, customer.card)
        val flashMsgOpt = request.flash.get("error").map(FlashMessage.error)

        Ok(views.html.tier.upgrade.paidToPaid(
          summary,
          privateFields,
          pageInfo,
          flashMsgOpt)(getToken, request.request))
      }
    }

    tp.subscriptionService.currentSubscription(request.member).flatMap { sub =>
      (sub, request.member) match {
        case (sub: FreeSubscription, Contact(d, t: FreeTierMember, p: NoPayment)) =>
          previewFreeUpgrade(sub, Contact(d, t, p))
        case (sub: PaidSubscription, Contact(d, t: PaidTierMember, p: StripePayment)) =>
          previewPaidUpgrade(sub, Contact(d, t, p))
        case _ =>
          val msg = s"Zuora account ${sub.accountId} is inconsistent with its corresponding Salesforce information"
          throw new IllegalStateException(msg)
      }
    }
  }

  def upgradeConfirm(target: PaidTier) = ChangeToPaidAction(target).async { implicit request =>
    val identityRequest = IdentityRequest(request)

    def handleFree(freeMember: Contact[Member, NoPayment])(form: FreeMemberChangeForm) = for {
      memberId <- MemberService.upgradeFreeSubscription(freeMember, target, form, identityRequest, extractCampaignCode(request))
    } yield Ok(Json.obj("redirect" -> routes.TierController.upgradeThankyou(target).url))

    def handlePaid(paidMember: Contact[Member, StripePayment])(form: PaidMemberChangeForm) = {
      val reauthFailedMessage: Future[Result] = Future {
        Redirect(routes.TierController.upgrade(target))
          .flashing("error" ->
          s"That password does not match our records. Please try again.")
      }

      def doUpgrade(): Future[Result] = {
        MemberService.upgradePaidSubscription(paidMember, target, identityRequest, extractCampaignCode(request), form).map {
          _ => Redirect(routes.TierController.upgradeThankyou(target))
        }
      }

      for {
        status <- IdentityService(IdentityApi).reauthUser(paidMember.email, form.password, identityRequest)
        result <- if (status == 200) doUpgrade() else reauthFailedMessage
      } yield result

    }

    val futureResult = request.member match {
      case Contact(d, c, p: NoPayment) => freeMemberChangeForm.bindFromRequest.fold(redirectToUnsupportedBrowserInfo, handleFree(Contact(d, c, p)))
      case Contact(d, c, p: StripePayment) => paidMemberChangeForm.bindFromRequest.fold(redirectToUnsupportedBrowserInfo, handlePaid(Contact(d, c, p)))
    }

    futureResult.map(_.discardingCookies(DiscardingCookie("GU_MEM"))).recover {
      case error: Stripe.Error => Forbidden(Json.toJson(error))
      case error: ResultError => Forbidden
      case error: ScalaforceError => Forbidden
    }
  }

  def upgradeThankyou(tier: PaidTier) = Joiner.thankyou(tier, upgrade=true)
}

trait CancelTier {
  self: TierController =>

  def cancelTier() = MemberAction.async { implicit request =>
    request.catalog.map { catalog =>
      Ok(views.html.tier.cancel.confirm(request.member.tier, catalog))
    }
  }

  def cancelTierConfirm() = MemberAction.async { implicit request =>
    request.touchpointBackend.cancelSubscription(request.member, request.user, extractCampaignCode(request)).map { _ =>
      request.member.tier match {
        case m: FreeTierMember => Redirect(routes.TierController.cancelFreeTierSummary(m.tier))
        case _ => Redirect(routes.TierController.cancelPaidTierSummary())
      }
    }
  }

  def cancelFreeTierSummary(freeTier: FreeTier) = AuthenticatedAction(
    Ok(views.html.tier.cancel.summaryFree(freeTier))
  )


  def cancelPaidTierSummary = PaidMemberAction.async { implicit request =>
    request.touchpointBackend.subscriptionService.currentPaidSubscription(request.member).map { sub =>
      Ok(views.html.tier.cancel.summaryPaid(sub))
    }
  }
}

trait TierController extends Controller with UpgradeTier with DowngradeTier with CancelTier {
  def change() = MemberAction.async { implicit request =>
    implicit val currency = GBP
    val catalog = request.catalog
    for {
      cat <- catalog
    } yield {
      Ok(views.html.tier.change(request.member.tier, cat))
    }
  }
}

object TierController extends TierController
