package controllers

import scala.concurrent.Future

import play.api.mvc.{Controller, Request, Result}
import play.api.libs.json.Json
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import com.netaporter.uri.dsl._

import com.gu.membership.salesforce.{PaidMember, ScalaforceError, Tier}

import actions._
import configuration.{Config, CopyConfig}
import forms.MemberForm.{JoinForm, friendJoinForm, paidMemberJoinForm, staffJoinForm}
import model.Eventbrite.{EBCode, RichEvent}
import model._
import model.StripeSerializer._
import services._

trait Joiner extends Controller {

  val memberService: MemberService

  def tierList = CachedAction { implicit request =>
    val pageInfo = PageInfo(
      CopyConfig.copyTitleJoin,
      request.path,
      Some(CopyConfig.copyDescriptionJoin)
    )
    Ok(views.html.joiner.tierList(pageInfo))
  }

  def staff = GoogleAuthenticatedStaffNonMemberAction.async { implicit request =>
    val error = request.flash.get("error")
    val userSignedIn = AuthenticationService.authenticatedUserFor(request)

    userSignedIn match {
      case Some(user) => for {
        fullUser <- IdentityService.getFullUserDetails(user, IdentityRequest(request))
        primaryEmailAddress = fullUser.primaryEmailAddress
        displayName = fullUser.publicFields.displayName
        avatarUrl = fullUser.privateFields.socialAvatarUrl
      } yield {
        Ok(views.html.joiner.staff(new StaffEmails(request.user.email, Some(primaryEmailAddress)), displayName, avatarUrl, error))
      }
      case _ => Future.successful(Ok(views.html.joiner.staff(new StaffEmails(request.user.email, None), None, None, error)))
    }
  }

  def enterDetails(tier: Tier.Tier) = AuthenticatedNonMemberAction.async { implicit request =>
    for {
      (privateFields, marketingChoices, passwordExists) <- identityDetails(request.user, request)
    } yield {

      tier match {
        case Tier.Friend => Ok(views.html.joiner.detail.addressForm(privateFields, marketingChoices, passwordExists))
        case paidTier =>
          val pageInfo = PageInfo.default.copy(stripePublicKey = Some(request.touchpointBackend.stripeService.publicKey))
          Ok(views.html.joiner.payment.paymentForm(paidTier, privateFields, marketingChoices, passwordExists, pageInfo))
      }

    }
  }

  def enterStaffDetails = GoogleAndIdentityAuthenticatedStaffNonMemberAction.async { implicit request =>
    for {
      (privateFields, marketingChoices, passwordExists) <- identityDetails(request.identityUser, request)
    } yield {
      Ok(views.html.joiner.detail.addressForm(privateFields, marketingChoices, passwordExists))
    }
  }

  private def identityDetails(user: IdMinimalUser, request: Request[_]) = {
    val identityRequest = IdentityRequest(request)
    for {
      user <- IdentityService.getFullUserDetails(user, identityRequest)
      passwordExists <- IdentityService.doesUserPasswordExist(identityRequest)
    } yield (user.privateFields, user.statusFields, passwordExists)
  }

  def joinFriend() = AuthenticatedNonMemberAction.async { implicit request =>
    friendJoinForm.bindFromRequest.fold(_ => Future.successful(BadRequest),
      makeMember { Redirect(routes.Joiner.thankyou(Tier.Friend)) } )
  }

  def joinStaff() = AuthenticatedNonMemberAction.async { implicit request =>
    staffJoinForm.bindFromRequest.fold(_ => Future.successful(BadRequest),
        makeMember { Redirect(routes.Joiner.thankyouStaff()) } )
    }

  def joinPaid(tier: Tier.Tier) = AuthenticatedNonMemberAction.async { implicit request =>
    paidMemberJoinForm.bindFromRequest.fold(_ => Future.successful(BadRequest),
      makeMember { Ok(Json.obj("redirect" -> routes.Joiner.thankyou(tier).url)) } )
  }

  private def makeMember(result: Result)(formData: JoinForm)(implicit request: AuthRequest[_]) = {
    MemberService.createMember(request.user, formData, IdentityRequest(request))
      .map { _ => result }
      .recover {
        case error: Stripe.Error => Forbidden(Json.toJson(error))
        case error: Zuora.ResultError => Forbidden
        case error: ScalaforceError => Forbidden
      }
  }

  def thankyou(tier: Tier.Tier, upgrade: Boolean = false) = MemberAction.async { implicit request =>
    def futureCustomerOpt = request.member match {
      case paidMember: PaidMember =>
        request.touchpointBackend.stripeService.Customer.read(paidMember.stripeCustomerId).map(Some(_))
      case _ => Future.successful(None)
    }

    def futureEventDetailsOpt = {
      val optFuture = for {
        eventId <- PreMembershipJoiningEventFromSessionExtractor.eventIdFrom(request)
        event <- EventbriteService.getBookableEvent(eventId)
      } yield {
        MemberService.createDiscountForMember(request.member, event).map { discountOpt =>
          (event, (Config.eventbriteApiIframeUrl ? ("eid" -> event.id) & ("discount" -> discountOpt.map(_.code))).toString)
        }
      }

      Future.sequence(optFuture.toSeq).map(_.headOption)
    }

    for {
      subscription <- request.touchpointBackend.subscriptionService.getCurrentSubscriptionDetails(request.member.salesforceAccountId)
      customerOpt <- futureCustomerOpt
      eventDetailsOpt <- futureEventDetailsOpt
    } yield Ok(views.html.joiner.thankyou(request.member, subscription, customerOpt.map(_.card), eventDetailsOpt, upgrade))
  }

  def thankyouStaff = thankyou(Tier.Friend)
}

object Joiner extends Joiner {
  val memberService = MemberService
}
