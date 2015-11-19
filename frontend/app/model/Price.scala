package model

import com.gu.i18n.Currency

case class Price(amount: Int, currency: Currency) {
  lazy val pretty = currency.glyph + amount.toString
  def +(n: Int) = Price(amount + n, currency)
  def -(n: Int) = Price(amount - n, currency)
  def *(n: Int) = Price(amount * n, currency)
  def /(n: Float) = Price((amount / n).toInt, currency)
}
