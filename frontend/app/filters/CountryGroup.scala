package filters

import com.gu.i18n.Currency
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{Cookie, Filter, RequestHeader, Result}
import services.{AuthenticationService, TouchpointBackend}

import scala.concurrent.Future
import scalaz.OptionT
import scalaz.std.scalaFuture._

object CountryGroup extends Filter {
  val currencyKey = "gu-currency"
  val countryGroupKey = "gu-country-group"
  val cookieMaxAge = 30 * 24 * 3600 // 1 month

  // if a currency is present in the session    => return
  // else if the user is a member               => add the currency to the session
  // else if there is a "currency" query string => add the country group to the session
  // else                                       => return
  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    def result = f(rh)

    def addingCurrency(c: Currency): Future[Result] =
      result.map(_.withCookies(Cookie(currencyKey, c.toString, maxAge = Some(cookieMaxAge), httpOnly = false)))

    def addingCountryGroup(countryGroupId: String): Future[Result] =
      result.map(_.withCookies(Cookie(countryGroupKey, countryGroupId, maxAge = Some(cookieMaxAge), httpOnly = false)))

    def memberCurrency: Future[Option[Currency]] = (for {
        userId <- OptionT(Future(AuthenticationService.authenticatedUserFor(rh)))
        tp = TouchpointBackend.forUser(userId)
        c <- OptionT(tp.memberRepository.getMember(userId.user.id))
        account <- OptionT(tp.subscriptionService.account(c))
        currency <- OptionT(Future(Currency.fromString(account.currency)))
      } yield currency).run

    rh.cookies.get(currencyKey).fold[Future[Result]] {
      memberCurrency.flatMap(_.map(addingCurrency).getOrElse(
        rh.getQueryString(countryGroupKey)
          .map(addingCountryGroup)
          .getOrElse(result)
      ))
    }(_ => result)
  }
}
