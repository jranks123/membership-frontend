package views.support

import com.gu.i18n.{Country, CountryGroup, Currency}
import configuration.{Config, CopyConfig}
import model.EventSchema
import play.api.libs.json.{JsString, JsValue, Json, Writes}
import views.support.PageInfo.FormI18n

case class PageInfo(title: String = CopyConfig.copyTitleDefault,
                    url: String = "/",
                    description: Option[String] = Some(CopyConfig.copyDescriptionDefault),
                    image: Option[String] = Some(Config.membershipUrl + Asset.at("images/common/mem-promo.jpg")),
                    schemaOpt: Option[EventSchema] = None,
                    customSignInUrl: Option[String] = None,
                    stripePublicKey: Option[String] = None,
                    formI18n: FormI18n = FormI18n.lockingCurrency(CountryGroup.UK.defaultCountry, CountryGroup.UK.currency)) {
    def bindingCurrency: PageInfo =
      copy(
        formI18n = formI18n.copy(
          lockCurrency = false
        )
      )
  }

object PageInfo {
  case class FormI18n(defaultCountry: Option[Country],
                      currency: Currency,
                      // Determines whether the currency should vary depending on the selected country
                      // e.g. set to false during upgrades
                      lockCurrency: Boolean)
  object FormI18n {
    def bindingCurrency(countryGroup: CountryGroup) =
      FormI18n(countryGroup.defaultCountry, countryGroup.currency, lockCurrency = false)

    def lockingCurrency(country: Option[Country], currency: Currency) =
      FormI18n(country, currency, lockCurrency = true)
  }

  implicit val countryWrites = new Writes[Country] {
    override def writes(c: Country): JsValue = JsString(c.alpha2)
  }

  implicit val currencyWrites = new Writes[Currency] {
    override def writes(c: Currency): JsValue = JsString(c.toString)
  }

  implicit val checkoutFormWrites = Json.writes[FormI18n]
}
