package model.Healthcheck
import com.gu.membership.salesforce.Tier
import model.{Pricing, TierPricing}
import scala.util.Try

case class PricingCheck(tp: () => Option[Try[TierPricing]]) extends HealthCheck {

  private def byTier: Option[Either[Map[Tier, List[String]], Map[Tier, Pricing]]] =
    tp().flatMap(_.toOption.map(_.byTier))

  override def ok: Boolean = byTier.exists(_.isRight)

  override def messages: Seq[String] = for {
    pricing <- byTier.toSeq
    errorReport <- pricing.left.toSeq
    (key, value) <- errorReport
  } yield s"$key: ${value mkString ", "}"
}
