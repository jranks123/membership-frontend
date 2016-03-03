package controllers

import com.gu.i18n._
import com.gu.membership.MembershipCatalog
import com.gu.memsub.Price
import com.gu.salesforce.PaidTier
import play.api.libs.json.{JsString, JsValue, Writes, Json}
import play.api.mvc.Controller
import services.TouchpointBackend
import views.support.{Pricing, CountryWithCurrency}

trait PricingWrites {
  implicit val currencyWrites = new Writes[Currency] {
    override def writes(currency: Currency): JsValue = currency match {
      case GBP => JsString("GBP")
      case AUD => JsString("AUD")
      case EUR => JsString("EUR")
      case NZD => JsString("NZD")
      case CAD => JsString("CAD")
      case USD => JsString("USD")
    }
  }
}

object CountryWithCurrencyResponse extends PricingWrites {
  implicit val countryWrites = Json.writes[Country]
  implicit val countryCurrencyWrites = Json.writes[CountryWithCurrency]
}

case class PaidMembershipPlansResponse(tier: PaidTier, pricing: Pricing)

object PaidMembershipPlansResponse extends PricingWrites {
  implicit val priceWrites = new Writes[Price] {
    override def writes(price: Price): JsValue = Json.obj(
      "amount" -> f"${price.amount}%.2f",
      "currency" -> price.currency
    )
  }
  implicit val pricingWrites = Json.writes[Pricing]
  implicit val tierWrites = new Writes[PaidTier] {
    override def writes(tier: PaidTier): JsValue = JsString(tier.name)
  }
  implicit  val writes = Json.writes[PaidMembershipPlansResponse]
}

object PricingApi extends Controller {

  import CountryWithCurrencyResponse._
  import PaidMembershipPlansResponse._
  import views.support.Pricing._

  val membersCatalog: MembershipCatalog = TouchpointBackend.Normal.catalog

  def currencies = CachedAction {
    Ok(Json.toJson(CountryWithCurrency.all))
  }

  def paidPlans(currency: Currency) = CachedAction {
    val plansByTier = for {
      tier <- PaidTier.all
      plan = membersCatalog.findPaid(tier)
    } yield PaidMembershipPlansResponse(plan.tier, plan.pricingByCurrencyOrGBP(currency))
    Ok(Json.toJson(plansByTier))
  }
}

