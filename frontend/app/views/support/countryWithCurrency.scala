package views.support

import com.gu.i18n._

case class CountryWithCurrency(country: Country, currency: Currency)

object CountryWithCurrency {
  val all = CountryGroup.allGroups.flatMap(group =>
    group.countries.map(c => CountryWithCurrency(c, group.currency)))
}
