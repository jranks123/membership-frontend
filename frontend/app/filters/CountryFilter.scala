package filters

import actions.countryCodeKey
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.Future

object CountryFilter extends Filter {
  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    rh.getQueryString(countryCodeKey).fold(f(rh)) { code =>
      f(rh).map(_.addingToSession(countryCodeKey -> code)(rh))
    }
  }
}
