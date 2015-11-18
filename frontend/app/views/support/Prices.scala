package views.support

import com.gu.i18n.Currency
import com.gu.membership.zuora.soap.models.SubscriptionDetails

object Prices {
  implicit class RichFloat(price: Float) {
    def pretty(implicit currency: Currency) = currency.glyph + "%.2f".format(price)
  }
  implicit class RichInt(price: Int) {
    def pretty(implicit currency: Currency) = currency.glyph + price
  }

  implicit class SubscriptionDetailsWithPrettyPrices(subs: SubscriptionDetails) {
    def prettyPrice = subs.planAmount.pretty(subs.currency)
  }
}
