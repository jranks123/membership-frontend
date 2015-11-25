package views.support

import com.gu.i18n

case class CountryWithCurrency(country: i18n.Country, currency: i18n.Currency)

object CountryWithCurrency {
  val all = i18n.CountryGroup.allGroups.flatMap(group =>
    group.countries.map(c => CountryWithCurrency(c, group.currency)))
}
