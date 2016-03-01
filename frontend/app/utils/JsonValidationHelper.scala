package utils


import com.gu.i18n.CountryGroup
import play.api.data.validation.ValidationError
import play.api.libs.json.Reads
import play.api.libs.json.Reads.{minLength, filter}

object JsonValidationHelper {
  def nonEmptyString(implicit r: Reads[String]) = minLength[String](1)
  def countryCode(implicit r: Reads[String]) = filter[String](ValidationError("unknown.country.code"))(CountryGroup.byCountryCode(_).isDefined)
}
