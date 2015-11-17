package filters

import actions.countryGroupKey
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.Future

object CountryGroup extends Filter {
  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    rh.getQueryString(countryGroupKey).fold(f(rh)) { code =>
      f(rh).map(_.addingToSession(countryGroupKey -> code)(rh))
    }
  }
}
