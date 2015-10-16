
package controllers

import com.gu.membership.salesforce.Tier
import com.gu.monitoring.CloudWatchHealth
import model.{Pricing, TierPricing}
import play.api.Logger.warn
import play.api.mvc.{Action, Controller}
import services.{GuardianLiveEventService, TouchpointBackend}
import com.github.nscala_time.time.Imports._
import Function.const
import scala.util.Success

trait Test {
  def ok: Boolean
  def messages: Seq[String] = Nil
}

class BoolTest(name: String, exec: () => Boolean) extends Test {
  override def messages = List(s"Test $name failed, health check will fail")
  override def ok = exec()
}

class TierPricingTest extends Test {

  def getTierPricing: Option[Either[Map[Tier, List[String]], Map[Tier, Pricing]]] =
    TouchpointBackend.Normal.subscriptionService.tierPricing.value.flatMap(_.toOption.map(_.byTier))

  override def ok: Boolean = getTierPricing.exists(_.isRight)

  override def messages: Seq[String] =
    getTierPricing.fold[Seq[String]](Nil)(eReport =>
      eReport.left.toSeq.flatMap { report => report.collect { case (tier, errors) =>
          s"Cannot find some price info for tier $tier. Errors: ${errors.mkString(", ")}"
        }
      }
    )
}

object Healthcheck extends Controller {
  val zuoraSoapClient = TouchpointBackend.Normal.zuoraSoapClient
  val subscriptionService = TouchpointBackend.Normal.subscriptionService

  def tests = Seq(
    new BoolTest("Events", () => GuardianLiveEventService.events.nonEmpty),
    new BoolTest("CloudWatch", () => CloudWatchHealth.hasPushedMetricSuccessfully),
    new BoolTest("ZuoraPing", () => zuoraSoapClient.lastPingTimeWithin(2.minutes)),
    new BoolTest("ZuoraProductRatePlans", () => subscriptionService.productRatePlanIdSupplier.get().value.exists(_.isSuccess)),
    new TierPricingTest
  )

  def healthcheck() = Action {
    Cached(1) {
      val failures = tests.filterNot(_.ok)
      if (failures.isEmpty) {
        Ok("OK")
      } else {
        failures.foreach(_.messages.foreach(warn))
        ServiceUnavailable("Service Unavailable")
      }
    }
  }
}
