import filters._
import model.TierPricing
import monitoring.SentryLogging
import play.api.Application
import play.api.mvc.WithFilters
import play.filters.csrf._
import services._
import scala.concurrent.ExecutionContext.Implicits.global

object Global extends WithFilters(RedirectMembersFilter, CheckCacheHeadersFilter, CSRFFilter(), Gzipper, AddEC2InstanceHeader) {
  override def onStart(app: Application) {
    SentryLogging.init()

    GuardianLiveEventService.start()
    LocalEventService.start()
    MasterclassEventService.start()

    GuardianContentService.start()
    TouchpointBackend.TestUser.subscriptionService.membershipProducts.foreach { catalog =>
      val pricing = new TierPricing(catalog)
      println(pricing.byTier)
    }
  }
}
