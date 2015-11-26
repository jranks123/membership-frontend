package views.support

import com.gu.membership.model.{BillingPeriod, Price}
import org.joda.time.DateTime

case class ThankyouSummary(startDate: DateTime,
                           amountPaidToday: Price,
                           planAmount: Price,
                           nextPaymentPrice: Price,
                           nextPaymentDate: DateTime,
                           renewalDate: DateTime,
                           initialFreePeriodOffer: Boolean,
                           billingPeriod: BillingPeriod) {

  require(Set(amountPaidToday, planAmount, nextPaymentPrice).map(_.currency).size == 1)
}
