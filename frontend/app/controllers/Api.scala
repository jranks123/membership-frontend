package controllers


import actions.{RichAuthRequest, _}
import com.gu.identity.play.IdMinimalUser
import com.gu.memsub.Address
import com.gu.memsub.util.WebServiceHelperError
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

  def joinPreview = AuthenticatedApiAction.async(BodyJson[ApiJoinPreviewRequest]) { implicit request =>
    val apiRequest = request.body
    val res = ApiJoinPreviewResponse(Price(50000, "GBP"), apiRequest.planChoice, "THIS_DOESNT_WORK_YET")
    Future.successful(Ok(Json.toJson(res)))
  }

  def join = AuthenticatedApiAction.async(BodyJson[ApiJoinRequest]) { implicit request =>
    val apiRequest = request.body
    val planChoice = apiRequest.planChoice

    val form = PaidMemberJoinForm(
      tier = planChoice.tier,
      name = NameForm(apiRequest.firstName, apiRequest.lastName),
      payment = getPaymentForm(apiRequest),
      deliveryAddress = toAddress(apiRequest.deliveryAddress),
      billingAddress = apiRequest.billingAddress.map(toAddress),
      marketingChoices = MarketingChoicesForm(None, None),
      password = Some(apiRequest.password),
      casId = None,
      subscriberOffer = false,
      featureChoice = Set.empty,
      suppliedPromoCode = None
    )

    makeMember( Ok(Json.obj("message" -> "OK")))(form)

  }

  //TODO IS THERE A BETTER WAY OF DOING THIS?
  private def toAddress(apiAddress: ApiAddress): Address = Address(
    lineOne = apiAddress.lineOne,
    lineTwo = apiAddress.lineTwo.getOrElse(""),
    town = apiAddress.town,
    countyOrState = apiAddress.countyOrState.getOrElse(""),
    postCode = apiAddress.postCode,
    countryName = apiAddress.countryName
  )

  private def getPaymentForm(apiRequest: ApiRequest) = apiRequest.payment match {
    case s: StripePayment => PaymentForm(apiRequest.planChoice.billingPeriod, s.token)
    case dd: DirectDebit => PaymentForm(apiRequest.planChoice.billingPeriod, "THIS IS WRONG!") // TODO see how to do this properly( is there a way to do this?)
  }

  private def makeMember(onSuccess: => Result)(formData: JoinForm)(implicit request: AuthRequest[_]) = {
    implicit val bp: BackendProvider = request
    memberService.createMember(request.user, formData, IdentityRequest(request), None, None)
      .map(_ => onSuccess) recover {
      case error: Stripe.Error => Forbidden(Json.toJson(error))
        //TODO THIS JUST RE-THROWS THE ERRORS RETURNED BY ANY WEBSERVICE BUT THE STATUS FOR EXAMPLE COULD NOT BE THE CORRECT NOW (FOR EXAMPLE BAD REQUEST FOR THE WEB SERVICE FOR NOT THE API)
      case error: WebServiceHelperError[_] => new Status(error.responseCode)(error.responseBody)
      case error =>
        logger.error("An error occurred while calling api.makeMember", error)
        InternalServerError
    }
  }

}
