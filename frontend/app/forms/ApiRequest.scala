package forms

import com.gu.memsub._

import com.gu.salesforce.PaidTier
import com.gu.salesforce.Tier.{Patron, Partner, Supporter}
import play.api.libs.json._

sealed trait Payment {
}

case class StripePayment(token: String) extends Payment

case class DirectDebit(bankAccount: String, sortCode: String) extends Payment

case class Plan(tier: PaidTier, billingPeriod: BillingPeriod)

trait ApiRequest {
  val deliveryAddress: Address
  val billingAddress: Option[Address]
  val planChoice: Plan
  val payment: Payment
}

case class ApiJoinPreviewRequest(
                                  deliveryAddress: Address,
                                  billingAddress: Option[Address],
                                  planChoice: Plan,
                                  payment: Payment
                                ) extends ApiRequest

case class ApiJoinRequest(
                           firstName: String,
                           lastName: String,
                           password: String,
                           deliveryAddress: Address,
                           billingAddress: Option[Address],
                           planChoice: Plan,
                           payment: Payment,
                           planIdentifier: String // TODO I don't think this name is descriptive enough (this is the id the preview returns so that you know what you are paying for hasn't changed)
                         ) extends ApiRequest

case class Price(penceAmount: Int, currency: String)

case class ApiJoinPreviewResponse(
        price : Price,
        plan :Plan,
        planIdentifier:String) //TODO should this be the name?


object Plan {
  implicit val paidTierFormat = new Format[PaidTier] {
    override def writes(p: PaidTier): JsValue = JsString(p.name)

    override def reads(json: JsValue): JsResult[PaidTier] = json match {
      case JsString("Supporter") => JsSuccess(Supporter())
      case JsString("Partner") => JsSuccess(Partner())
      case JsString("Patron") => JsSuccess(Patron())

      case _ => JsError("Unknown tier")
    }
  }

  implicit val BillingPeriodFormat = new Format[BillingPeriod] {
    override def writes(b: BillingPeriod): JsValue = JsString(b.noun)

    override def reads(json: JsValue): JsResult[BillingPeriod] = json match {
      case JsString("month") => JsSuccess(Month())
      case JsString("year") => JsSuccess(Year())
      case JsString("quarter") => JsSuccess(Quarter())

      case _ => JsError("Unknown billing period")
    }
  }
  implicit val planFormat = Json.format[Plan]

}

object StripePayment {
  implicit val jf = Json.format[StripePayment]
}

object DirectDebit {
  implicit val jf = Json.format[DirectDebit]
}

object Payment {
  implicit val jf = new Format[Payment] {
    override def writes(o: Payment): JsValue = o match {
      case n: StripePayment => StripePayment.jf.writes(n)
      case n: DirectDebit => DirectDebit.jf.writes(n)
    }

    override def reads(json: JsValue): JsResult[Payment] = {
      json \ "type" match {
        case JsDefined(JsString("stripe")) => StripePayment.jf.reads(json)
        case JsDefined(JsString("directDebit")) => DirectDebit.jf.reads(json)
        case _ => JsError("Unknown payment type")
      }
    }
  }
}

object ApiJoinRequest {
  implicit val addressFormat = Json.format[Address]
  implicit val ApiJoinRequestFormat = Json.format[ApiJoinRequest]
}

object ApiJoinPreviewRequest {
  implicit val addressFormat = Json.format[Address]
  implicit val ApiJoinPreviewRequestFormat = Json.format[ApiJoinPreviewRequest]
}

object Price{
  implicit val jf = Json.format[Price]
}
object ApiJoinPreviewResponse{
  implicit val jf = Json.format[ApiJoinPreviewResponse]
}
