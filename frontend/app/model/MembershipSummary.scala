package model

import com.gu.i18n.Currency
import org.joda.time.DateTime

case class MembershipSummary(startDate: DateTime,
                             amountPaidToday: Option[Float],
                             currency: Currency,
                             planAmount: Float,
                             nextPaymentPrice: Float,
                             nextPaymentDate: DateTime,
                             renewalDate: DateTime) {

  val initialFreePeriodOffer = amountPaidToday.isEmpty

  val annual = startDate.plusYears(1).toLocalDate == renewalDate.toLocalDate

}
