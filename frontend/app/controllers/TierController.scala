package controllers

import actions._
import com.gu.i18n.GBP
import com.gu.identity.play.PrivateFields
import com.gu.membership.salesforce._
import com.gu.membership.stripe.Stripe
import com.gu.membership.stripe.Stripe.Serializer._
import com.gu.membership.zuora.soap.models.SubscriptionDetails
import com.gu.membership.zuora.soap.models.errors.ResultError
import forms.MemberForm._
import model.{FlashMessage, PageInfo}
import org.joda.time.DateTime
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.mvc.{Controller, DiscardingCookie, Result}
import play.filters.csrf.CSRF.Token.getToken
import services._
import tracking.ActivityTracking
import utils.CampaignCode.extractCampaignCode
import views.support.UpgradeSummary

import scala.concurrent.Future

trait DowngradeTier extends ActivityTracking {
  self: TierController =>

  def downgradeToFriend() = PaidMemberAction.async { implicit request =>
    for {
      cat <- request.catalog
      subsDetails <- request.touchpointBackend.subscriptionService.getCurrentSubscriptionDetails(request.member)
    } yield {
      Ok(views.html.tier.downgrade.confirm(cat.unsafePaidTierPlan(subsDetails.productRatePlanId).tier, cat))
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
      subscription <- subscriptionService.getCurrentSubscriptionDetails(request.member)
      cat <- catalogF
    } yield {
      val startDate = subscription.chargedThroughDate.map(_.plusDays(1)).getOrElse(DateTime.now)
      Ok(views.html.tier.downgrade.summary(subscription, currentTier, cat, startDate))
    }
  }
}

trait UpgradeTier {
  self: TierController =>

  def upgrade(target: PaidTier) = ChangeToPaidAction(target).async { implicit request =>
    val tp = request.touchpointBackend
    implicit val currency = GBP

    def previewUpgrade(subscription: SubscriptionDetails): Future[Result] = {
      if (subscription.inFreePeriodOffer) Future.successful(Ok(views.html.tier.upgrade.unavailable(request.member.tier, target)))
      else {
        val identityUserFieldsF = IdentityService(IdentityApi).getFullUserDetails(request.user, IdentityRequest(request)).map(_.privateFields.getOrElse(PrivateFields()))
        val catalog = request.catalog
        val pageInfo = PageInfo.default.copy(stripePublicKey = Some(tp.stripeService.publicKey))

        request.member match {
          case Contact(d, c@PaidTierMember(_, _), p: StripePayment) =>
            val contact = Contact(d, c, p)
            val stripeCustomerF = tp.stripeService.Customer.read(contact.stripeCustomerId)

            for {
              (account, restSub) <- tp.subscriptionService.latestMembershipSubscription(request.member)
              subs = SubscriptionDetails(restSub)
              previewItems <- MemberService.previewUpgradeSubscription(subs, contact, target, tp)
              cat <- catalog
              customer <- stripeCustomerF
              privateFields <- identityUserFieldsF
            } yield {
              val summary = UpgradeSummary(cat, previewItems, restSub, target, customer.card)
              val flashMsgOpt = request.flash.get("error").map(FlashMessage.error)

              Ok(views.html.tier.upgrade.paidToPaid(
                summary,
                privateFields,
                pageInfo,
                flashMsgOpt)(getToken, request.request))
            }
          case Contact(d, c@PaidTierMember(n, _), _) =>
            throw new IllegalStateException(s"Unexpected state: member number $n has a paid tier but no payment details")
          case Contact(d, c@FreeTierMember(_), _) =>
            for {
              privateFields <- identityUserFieldsF
              cat <- catalog
            } yield {
              val currentDetails = cat.freeTierDetails(c.tier)
              val targetDetails = cat.paidTierDetails(target)

              Ok(views.html.tier.upgrade.freeToPaid(currentDetails, targetDetails, privateFields, pageInfo)(getToken, request.request, currency))
            }
        }
      }
    }

    for {
      subscription <- tp.subscriptionService.getCurrentSubscriptionDetails(request.member)
      result <- previewUpgrade(subscription)
    } yield result
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
    for {
      _ <- request.touchpointBackend.cancelSubscription(request.member, request.user, extractCampaignCode(request))
    } yield {
      Redirect("/tier/cancel/summary")
    }
  }

  def cancelTierSummary() = AuthenticatedAction.async { implicit request =>
    def subscriptionDetailsFor(memberOpt: Option[Contact[Member, PaymentMethod]]) = {
      memberOpt.collect { case Contact(d, m, p: StripePayment) =>
        request.touchpointBackend.subscriptionService.getCurrentSubscriptionDetails(d)
      }
    }

    for {
      memberOpt <- request.touchpointBackend.memberRepository.getMember(request.user.id)
      subscriptionDetails <- Future.sequence(subscriptionDetailsFor(memberOpt).toSeq)
    } yield {
      val currentTierOpt = memberOpt.map(_.tier)
      Ok(views.html.tier.cancel.summary(subscriptionDetails.headOption, currentTierOpt))
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
