package controllers


import actions.{RichAuthRequest, _}
import com.gu.memsub.{Address => FormAddress}
import com.gu.memsub.util.WebServiceHelperError
import com.gu.stripe.Stripe
import com.gu.stripe.Stripe.Serializer._
import com.typesafe.scalalogging.LazyLogging
import forms.MemberForm._
import model.Api._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.mvc.BodyParsers.parse.{json => BodyJson}
import play.api.mvc._
import tracking.ActivityTracking

import scala.concurrent.Future

object Api extends Controller with ActivityTracking
  with LazyLogging
  with MemberServiceProvider {

  def joinPreview = AuthenticatedApiAction.async(BodyJson[JoinPreviewRequest]) { implicit request =>
    val apiRequest = request.body
    val res = ApiJoinPreviewResponse(Price(50000, "GBP"), apiRequest.planChoice, "THIS_DOESNT_WORK_YET")
    Future.successful(Ok(Json.toJson(res)))
  }

  def join = AuthenticatedApiAction.async(BodyJson[JoinRequest]) { implicit request =>
    val apiRequest = request.body
    val planChoice = apiRequest.planChoice

    val form = PaidMemberJoinForm(
      tier = planChoice.tier,
      name = NameForm(apiRequest.firstName, apiRequest.lastName),
      payment = getPaymentForm(apiRequest),
      deliveryAddress = toFormAddress(apiRequest.deliveryAddress),
      billingAddress = apiRequest.billingAddress.map(toFormAddress),
      marketingChoices = MarketingChoicesForm(None, None),
      password = Some(apiRequest.password),
      casId = None,
      subscriberOffer = false,
      featureChoice = Set.empty,
      suppliedPromoCode = None
    )

    makeMember( Ok(Json.obj("message" -> "OK")))(form)

  }

  implicit private def toFormAddress(apiAddress: Address): FormAddress = FormAddress(
    lineOne = apiAddress.lineOne,
    lineTwo = apiAddress.lineTwo.getOrElse(""),
    town = apiAddress.town,
    countyOrState = apiAddress.countyOrState.getOrElse(""),
    postCode = apiAddress.postCode,
    countryName = apiAddress.countryName
  )

  private def getPaymentForm(joinRequest: JoinRequest) = joinRequest.payment match {
    case s: StripePayment => PaymentForm(joinRequest.planChoice.billingPeriod, s.token)
    case dd: DirectDebit => PaymentForm(joinRequest.planChoice.billingPeriod, "THIS IS WRONG!") // TODO see how to do this properly( is there a way to do this?)
  }

  private def makeMember(onSuccess: => Result)(formData: JoinForm)(implicit request: AuthRequest[_]) = {
    implicit val bp: BackendProvider = request
    memberService.createMember(request.user, formData, IdentityRequest(request), None, None)
      .map(_ => onSuccess) recover {
      case error: Stripe.Error => Forbidden(Json.toJson(error))
      case error: WebServiceHelperError[_] => new Status(error.responseCode)(error.responseBody)
      case error =>
        logger.error("An error occurred while calling api.makeMember", error)
        InternalServerError
    }
  }

}
