package controllers

import com.gu.monitoring.CloudWatchHealth
import model.Healthcheck.{PricingCheck, BooleanCheck}
import play.api.Logger.warn
import play.api.mvc.{Action, Controller}
import services.{GuardianLiveEventService, TouchpointBackend}
import com.github.nscala_time.time.Imports._

object Healthcheck extends Controller {
  val zuoraSoapClient = TouchpointBackend.Normal.zuoraSoapClient
  val subscriptionService = TouchpointBackend.Normal.subscriptionService

  def tests = Seq(
    BooleanCheck("Events", () => GuardianLiveEventService.events.nonEmpty),
    BooleanCheck("CloudWatch", () => CloudWatchHealth.hasPushedMetricSuccessfully),
    BooleanCheck("ZuoraPing", () => zuoraSoapClient.lastPingTimeWithin(2.minutes)),
    BooleanCheck("ZuoraProductRatePlans", () => subscriptionService.productRatePlanIdSupplier.get().value.exists(_.isSuccess)),
    PricingCheck(() => TouchpointBackend.Normal.subscriptionService.tierPricing.value)
  )

  def healthcheck() = Action {
    Cached(1) {
      val failures = tests.filterNot(_.ok)
      if (failures.isEmpty) {
        Ok("OK")
      } else {
        failures.foreach(_.messages.foreach(warn(_)))
        ServiceUnavailable("Service Unavailable")
      }
    }
  }
}
