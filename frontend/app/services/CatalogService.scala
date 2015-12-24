package services

import akka.agent.Agent
import com.gu.config.Membership
import com.gu.membership.MembershipCatalog
import com.gu.membership.MembershipCatalog.Val
import com.gu.touchpoint.TouchpointBackendConfig.BackendType
import com.gu.zuora
import play.api.libs.concurrent.Akka
import com.gu.membership.services.{api => commonapi}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current

object CatalogService {
  def makeCatalog(restClient: zuora.rest.Client, productFamily: Membership)(implicit backendType: BackendType): Future[Val[MembershipCatalog]] = {
    restClient.productCatalog.map { catalog =>
      val productRatePlans = catalog.products.flatMap(_.productRatePlans)
      MembershipCatalog.fromZuora(productFamily)(productRatePlans)
    }
  }

  def apply(restClient: zuora.rest.Client, productFamily: Membership)(implicit backendType: BackendType): CatalogService = {
    def unsafeMakeCatalog: Future[MembershipCatalog] = makeCatalog(restClient, productFamily).map(_.fold(
      { errs => throw new RuntimeException(
        s"Failed creating the membership catalog for environment ${backendType.name}: ${errs.list.mkString(", ")}")
      },
      identity
    ))

    val agent = Agent(Await.result(unsafeMakeCatalog, 2.minutes))
    Akka.system.scheduler.schedule(0.milli, 5.minutes)(unsafeMakeCatalog.map(agent.send))
    new CatalogService(agent, productFamily)
  }
}

class CatalogService(agent: Agent[MembershipCatalog],
                     val productFamily: Membership
                     ) extends commonapi.CatalogService {
  override def catalog: MembershipCatalog = agent.get
}
