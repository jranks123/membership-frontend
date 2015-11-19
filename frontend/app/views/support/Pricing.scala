package views.support

import com.gu.i18n.Currency
import model.{PaidTierDetails, Price}


case class Pricing(yearly: Price, monthly: Price) {
  require(yearly.currency == monthly.currency, "The yearly and monthly prices should have the same currency")

  lazy val currency = monthly.currency
  lazy val yearlyMonthlyPrice = monthly * 12
  lazy val yearlySaving = yearlyMonthlyPrice - yearly.amount
  lazy val yearlyWith6MonthSaving = yearly / 2f
  lazy val hasYearlySaving = yearlySaving.amount > 0
  lazy val yearlySavingsInMonths = (yearly.amount - yearlyMonthlyPrice.amount) / monthly.amount

  val savingInfo: Option[String] =
    if (hasYearlySaving) Some(s"Save ${yearlySaving.pretty}/year") else None
}

object Pricing {
  implicit class WithPricing(td: PaidTierDetails) {
    def pricingWithFallback(implicit currency: Currency): Pricing =
      ( td.yearlyPlanDetails.pricingByCurrency.getPrice(currency),
        td.monthlyPlanDetails.pricingByCurrency.getPrice(currency)
      ) match {
        case (Some(y), Some(m)) =>
          Pricing(Price(y.toInt, currency), Price(m.toInt, currency))
        case _ =>
          Pricing(td.yearlyPlanDetails.priceGBP, td.monthlyPlanDetails.priceGBP)
      }
  }
}
