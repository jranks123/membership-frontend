package controllers

import actions._
import com.gu.cas.CAS.CASSuccess
import com.gu.membership.salesforce._
import com.gu.membership.zuora.soap.models.SubscriptionDetails
import model.Benefits
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, Instant}
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json.toJson
import play.api.libs.json._
import play.api.mvc.{Controller, Cookie}
import services.{MembersDataAPI, CASService}
import utils.GuMemCookie

import scala.concurrent.Future

trait User extends Controller {
  val standardFormat = ISODateTimeFormat.dateTime.withZoneUTC
  implicit val writesInstant = Writes[Instant] { instant => JsString(instant.toString(standardFormat)) }

  val casService = CASService

  def me = AjaxMemberAction { implicit request =>
    val json = basicDetails(request.member)
    request.idCookies.foreach(MembersDataAPI.Service.check(request.member.memberStatus))
    Ok(json).withCookies(Cookie("GU_MEM", GuMemCookie.encodeUserJson(json), secure = true, httpOnly = false))
  }

  def meDetails = ???

  def basicDetails(member: Contact[Member, PaymentMethod]) = Json.obj(
    "userId" -> member.identityId,
    "regNumber" -> member.memberStatus.regNumberLabel,
    "firstName" -> member.firstName,
    "tier" -> member.tier.name,
    "isPaidTier" -> member.tier.isPaid,
    "joinDate" -> member.joinDate,
    "benefits" -> Json.obj(
      "discountedEventTickets" -> Benefits.DiscountTicketTiers.contains(member.tier),
      "complimentaryEventTickets" -> Benefits.ComplimenataryTicketTiers.contains(member.tier)
    )
  )

  case class SubCheck(valid: Boolean, msg: Option[String] = None)
  object SubCheck { implicit val writesSubscriberResult = Json.writes[SubCheck] }

  def checkSubscriberDetails(id: String, postcode: Option[String], lastName: String) = AjaxAuthenticatedAction.async { implicit request =>
    val existingSubsWithCasIdF = request.touchpointBackend.subscriptionService.getSubscriptionsByCasId(id)
    for {
      casResult <- casService.check(id, postcode, lastName)
      existingSubsWithCasId <- existingSubsWithCasIdF
    } yield {
      val subCheck = casResult match {
        case casSuccess: CASSuccess =>
          if (new DateTime(casSuccess.expiryDate).isBeforeNow) SubCheck(false, Some("Sorry, your subscription has expired."))
          else if (existingSubsWithCasId.nonEmpty) SubCheck(false, Some(s"Sorry, the Subscriber ID entered has already been used to redeem this offer."))
          else SubCheck(true)
        case _ => SubCheck(false, Some(s"To redeem this offer we need more information to validate your Subscriber ID.  Please review the additional information required and try again."))
      }
      if (!subCheck.valid) {
        Logger.warn(s"sub user=${request.user.id} sub-id=$id cas=$casResult existing-subs=$existingSubsWithCasId $subCheck")
      }
      Ok(toJson(subCheck))
    }
  }
}

object User extends User
