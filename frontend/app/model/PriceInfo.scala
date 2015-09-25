package model

case class PriceInfo(id: String,
                       currency: String,
                       price: String,
                       priceFormat: Option[String],
                       endingUnit: Option[String])
