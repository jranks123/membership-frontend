package model

import com.gu.membership.stripe.Stripe
import com.gu.membership.zuora.soap.Zuora._
import com.gu.membership.zuora.soap.ZuoraReaders.{ZuoraQueryReader, ZuoraReader, ZuoraResultReader}
import org.joda.time.DateTime

object Zuora {
  case class AmendResult(ids: Seq[String], invoiceItems: Seq[PreviewInvoiceItem]) extends ZuoraResult

  //TODO: Below Queries/Results also exist to membership-common, needs resolving!!
  case class SubscribeResult(id: String) extends ZuoraResult
  case class Account(id: String, createdDate: DateTime) extends ZuoraQuery
  case class RatePlan(id: String, name: String, productRatePlanId: String) extends ZuoraQuery
  case class RatePlanCharge(id: String, chargedThroughDate: Option[DateTime], effectiveStartDate: DateTime,
                            price: Float) extends ZuoraQuery
  case class Subscription(id: String, version: Int, termStartDate: DateTime, contractAcceptanceDate: DateTime) extends ZuoraQuery



  case class Usage(description: String) extends ZuoraQuery

  trait Error extends Throwable {
    val code: String
    val message: String

    val fatal = true

    override def getMessage: String = s"$code: $message"
  }

  case class FaultError(code: String, message: String) extends Error
  case class ResultError(code: String, message: String) extends Error {
    override val fatal = {
      val cardDeclined = code == "TRANSACTION_FAILED"
      !cardDeclined
    }
  }
  case class InternalError(code: String, message: String) extends Error

  case class SubscriptionStatus(currentVersion: Subscription, futureVersionOpt: Option[Subscription], amendType: Option[String]) {
    val currentVersionId = currentVersion.id
    val futureVersionIdOpt = futureVersionOpt.map(_.id)
    val cancelled = amendType.contains("Cancellation")
  }

  case class SubscriptionDetails(planName: String, planAmount: Float, effectiveStartDate: DateTime,
                                 contractAcceptanceDate: DateTime, chargedThroughDate: Option[DateTime],
                                 ratePlanId: String) {

    val inFreePeriodOffer = chargedThroughDate.isEmpty && contractAcceptanceDate.isAfterNow

    val annual = chargedThroughDate.getOrElse(contractAcceptanceDate) == effectiveStartDate.plusYears(1)
    val paymentPeriodLabel = if (annual) "year" else "month"
  }

  object SubscriptionDetails {
    def apply(subscription: Subscription, ratePlan: RatePlan, ratePlanCharge: RatePlanCharge): SubscriptionDetails = {
      //val endDate = ratePlanCharge.chargedThroughDate.getOrElse(DateTime.now)

      // Zuora requires rate plan names to be unique, even though they are never used as identifiers
      // We want to show the same name for annual and monthly, so remove the " - annual" or " - monthly"
      val planName = ratePlan.name.split(" - ")(0)

      SubscriptionDetails(planName, ratePlanCharge.price, ratePlanCharge.effectiveStartDate, subscription.contractAcceptanceDate,
        ratePlanCharge.chargedThroughDate, ratePlan.id)
    }
  }

  case class PaymentSummary(current: InvoiceItem, previous: Seq[InvoiceItem]) {
    val totalPrice = current.price + previous.map(_.price).sum
  }

  object PaymentSummary {
    def apply(items: Seq[InvoiceItem]): PaymentSummary = {
      val sortedInvoiceItems = items.sortBy(_.chargeNumber)
      PaymentSummary(sortedInvoiceItems.last, sortedInvoiceItems.dropRight(1))
    }
  }
  case class PreviewInvoiceItem(price: Float, serviceStartDate: DateTime, serviceEndDate: DateTime, productId: String, unitPrice: Float) {
    val renewalDate = serviceEndDate.plusDays(1)
  }

  case class PaidPreview(card: Stripe.Card, invoiceItems: Seq[PreviewInvoiceItem]) {
    val sortedInvoiceItems = invoiceItems.sortBy(_.price)
    //We assume that this is only using paid-to-paid upgrades
    //    if we start supporting paid-to-pad downgrades we need to revisit this
    val futureSubscriptionInvoice = sortedInvoiceItems.last
    val totalPrice = invoiceItems.map(_.price).sum
  }
}


object ZuoraDeserializer {
  import model.Zuora._

  implicit val authenticationReader = ZuoraReader("loginResponse") { result =>
    Right(Authentication((result \ "Session").text, (result \ "ServerUrl").text))
  }

  implicit val amendResultReader = ZuoraResultReader.multi("amendResponse") { result =>
    val invoiceItems = (result \ "InvoiceDatas" \ "InvoiceItem").map {node =>
      PreviewInvoiceItem(
        (node \ "ChargeAmount").text.toFloat + (node \ "TaxAmount").text.toFloat,
        new DateTime((node \ "ServiceStartDate").text),
        new DateTime((node \ "ServiceEndDate").text),
        (node \ "ProductId").text,
        (node \ "UnitPrice").text.toFloat
      )
    }

    AmendResult((result \ "AmendmentIds").map(_.text), invoiceItems)
  }

  implicit val createResultReader = ZuoraResultReader("createResponse") { result =>
    CreateResult((result \ "Id").text)
  }


  implicit val usageReader = ZuoraQueryReader("Usage", Seq("Description")) { result =>
    Usage(result("Description"))
  }

  implicit val featureReader = ZuoraQueryReader("Feature", Seq("Id", "FeatureCode")) { result =>
    Feature(result("Id"), result("FeatureCode"))
  }

  implicit val subscribeResultReader = ZuoraResultReader("subscribeResponse") { result =>
    SubscribeResult((result \ "SubscriptionId").text)
  }

  implicit val updateResultReader = ZuoraResultReader("updateResponse") { result =>
    UpdateResult((result \ "Id").text)
  }

  implicit val accountReader = ZuoraQueryReader("Account", Seq("Id", "CreatedDate")) { result =>
    Account(result("Id"), new DateTime(result("CreatedDate")))
  }

  implicit val amendmentReader = ZuoraQueryReader("Amendment", Seq("Id", "Type", "ContractEffectiveDate", "SubscriptionId")) { result =>
    Amendment(result("Id"), result("Type"), new DateTime(result("ContractEffectiveDate")), result("SubscriptionId"))
  }

  implicit val invoiceItemReader = ZuoraQueryReader("InvoiceItem",
    Seq("Id", "ChargeAmount", "TaxAmount","ServiceStartDate", "ServiceEndDate", "ChargeNumber", "ProductName")) { result =>

    InvoiceItem(result("Id"), result("ChargeAmount").toFloat + result("TaxAmount").toFloat,
      new DateTime(result("ServiceStartDate")), new DateTime(result("ServiceEndDate")), result("ChargeNumber"),
      result("ProductName"))
  }

  implicit val ratePlanReader = ZuoraQueryReader("RatePlan", Seq("Id", "Name", "ProductRatePlanId")) { result =>
    RatePlan(result("Id"), result("Name"), result("ProductRatePlanId"))
  }

  implicit val ratePlanChargeReader = ZuoraQueryReader("RatePlanCharge", Seq("Id", "ChargedThroughDate", "EffectiveStartDate", "Price")) { result =>
    RatePlanCharge(result("Id"), result.get("ChargedThroughDate").map(new DateTime(_)),
      new DateTime(result("EffectiveStartDate")), result("Price").toFloat)
  }
  implicit val subscriptionReader = ZuoraQueryReader("Subscription", Seq("Id", "Version", "TermStartDate", "ContractAcceptanceDate")) { result =>
    Subscription(result("Id"), result("Version").toInt, new DateTime(result("TermStartDate")), new DateTime(result("ContractAcceptanceDate")))
  }
}
