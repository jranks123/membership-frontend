package views.support

import com.gu.i18n
import play.api.libs.json.Json

object CountryGroup {
  implicit class JsonCountryGroup(cg: i18n.CountryGroup) {
    def pageInfoJson = Json.obj(
      "country" -> cg.defaultCountry.map(_.alpha2),
      "currency" -> cg.currency.toString
    )
  }
}
