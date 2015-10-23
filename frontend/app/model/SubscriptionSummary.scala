package model
import model.SubscriptionSummary.Invoice
import org.joda.time.{Period, DateTime}

case class SubscriptionSummary(startDate: DateTime, invoices: Seq[Invoice], planAmount: Float) {
  val annual = new Period(invoices.head.date, invoices(1).date).getMonths > 1
}

object SubscriptionSummary {
  case class Invoice(date: DateTime, amount: Float)
}
