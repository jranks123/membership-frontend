package controllers


import actions.{RichAuthRequest, _}
import com.gu.identity.play.IdMinimalUser
import com.gu.salesforce._
import com.gu.stripe.Stripe
import com.gu.stripe.Stripe.Serializer._
import com.typesafe.scalalogging.LazyLogging
import forms.MemberForm._
import forms._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.mvc.BodyParsers.parse.{json => BodyJson}
import play.api.mvc._
import services._
import tracking.ActivityTracking
import utils.CampaignCode

import scala.concurrent.Future

object Api extends Controller with ActivityTracking
  with LazyLogging
  with MemberServiceProvider {
  def joinPreview(tier: Tier) = NoCacheAction.async(BodyJson[ApiJoinPreviewRequest]) { implicit request =>
    val apiRequest = request.body
     val res = ApiJoinPreviewResponse(Price(500,"GBP"), apiRequest.planChoice, "hVSCf8Pq7wfKL2R5")
     Future.successful(Ok(Json.toJson(res)))
  }
  def join(tier: Tier) = AuthenticatedApiAction.async(BodyJson[ApiJoinRequest]) { implicit request =>
    val apiRequest = request.body
    val planChoice = apiRequest.planChoice

    val form = PaidMemberJoinForm(
      tier = planChoice.tier,
      name = NameForm(apiRequest.firstName, apiRequest.lastName),
      payment = getPaymentForm(apiRequest),
      deliveryAddress = apiRequest.deliveryAddress,
      billingAddress = apiRequest.billingAddress,
      marketingChoices = MarketingChoicesForm(None, None),
      password = Some(apiRequest.password),
      casId = None,
      subscriberOffer = false,
      featureChoice = Set.empty,
      suppliedPromoCode = None
    )

    makeMember(tier, Ok(Json.obj("status" -> "OK", "message" -> "something else")))(form)
    // Future.successful(Ok(Json.obj("status" -> "OK", "message" -> "something")))

  }

  private def getPaymentForm(apiRequest: ApiRequest) = apiRequest.payment match {
    case s: StripePayment => PaymentForm(apiRequest.planChoice.billingPeriod, s.token)
    case dd: DirectDebit => PaymentForm(apiRequest.planChoice.billingPeriod, "THIS IS WRONG!") // TODO see how to do this properly
  }

  private def makeMember(tier: Tier, onSuccess: => Result)(formData: JoinForm)(implicit request: AuthRequest[_]) = {
    implicit val bp: BackendProvider = request
    memberService.createMember(request.user, formData, IdentityRequest(request), None, None)
      .map(_ => onSuccess) recover {
      case error: Stripe.Error => Forbidden(Json.toJson(error))
      case error =>
        logger.error("An error occurred while calling Joiner.makeMember", error)
        Forbidden
    }
  }

}
