package model

import com.gu.memsub.{Quarter, Year, Month, BillingPeriod}
import com.gu.salesforce.PaidTier
import com.gu.salesforce.Tier.{Partner, Patron, Supporter}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import utils.JsonValidationHelper._
package Api {

  sealed trait Payment

  case class StripePayment(token: String) extends Payment

  case class DirectDebit(bankAccount: String, sortCode: String) extends Payment

  case class Plan(tier: PaidTier, billingPeriod: BillingPeriod)

  case class Address(lineOne: String,
                     lineTwo: Option[String],
                     town: String,
                     countyOrState: Option[String],
                     postCode: String,
                     countryName: String)

  case class JoinPreviewRequest(deliveryAddress: Address,
                                billingAddress: Option[Address],
                                planChoice: Plan,
                                payment: Payment
                               )

  case class JoinRequest(firstName: String,
                         lastName: String,
                         password: String,
                         deliveryAddress: Address,
                         billingAddress: Option[Address],
                         planChoice: Plan,
                         payment: Payment
                        )

  case class Price(penceAmount: Int, currency: String)

  case class ApiJoinPreviewResponse(price: Price, plan: Plan, planIdentifier: String)


  object Address {

    implicit val AddressReads: Reads[Address] = (
      (__ \ "lineOne").read(nonEmptyString) and
        (__ \ "lineTwo").readNullable(nonEmptyString) and
        (__ \ "town").read(nonEmptyString) and
        (__ \ "countyOrState").readNullable(nonEmptyString) and
        (__ \ "postCode").read(minLength[String](1) andKeep maxLength[String](20)) and
        (__ \ "countryName").read(countryCode)
      ) (Address.apply _)

  }

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
    // implicit val planFormat = Format(Json.reads[Plan], Json.writes[Plan])

  }

  object StripePayment {
    implicit val stripePaymentReads: Reads[StripePayment] = (__ \ "token").read(nonEmptyString).map(StripePayment(_))
  }

  object DirectDebit {
    implicit val rs = Json.reads[DirectDebit]
  }

  object Payment {
    implicit val paymentReads = new Reads[Payment] {
      override def reads(json: JsValue): JsResult[Payment] = {
        json \ "type" match {
          case JsDefined(JsString("stripe")) => StripePayment.stripePaymentReads.reads(json)
          case JsDefined(JsString("directDebit")) => DirectDebit.rs.reads(json)
          case _ => JsError("Unknown payment type")
        }
      }
    }
  }

  object JoinRequest {
    implicit val joinRequestReads = (
      (__ \ "firstName").read(nonEmptyString) and
        (__ \ "lastName").read(nonEmptyString) and
        (__ \ "password").read(nonEmptyString) and
        (__ \ "deliveryAddress").read[Address] and
        (__ \ "billingAddress").readNullable[Address] and
        (__ \ "planChoice").read[Plan] and
        (__ \ "payment").read[Payment]
      ) (JoinRequest.apply _)

  }

  object JoinPreviewRequest {
    implicit val rs = Json.reads[JoinPreviewRequest]
  }

  object Price {
    implicit val jf = Json.format[Price]
  }

  object ApiJoinPreviewResponse {
    implicit val jf = Json.format[ApiJoinPreviewResponse]
  }

}
