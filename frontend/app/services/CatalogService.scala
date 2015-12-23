package services

import akka.agent.Agent
import com.gu.config.Membership
import com.gu.membership.MembershipCatalog
import com.gu.touchpoint.TouchpointBackendConfig.BackendType
import com.gu.zuora
import play.api.libs.concurrent.Akka
import com.gu.memsub.services.{api => commonapi}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

object CatalogService {
  def apply(restClient: zuora.rest.Client, productFamily: Membership)(implicit backendType: BackendType): CatalogService = {
    def makeCatalog: Future[MembershipCatalog] = {
      restClient.productCatalog.map { catalog =>
        val productRatePlans = catalog.products.flatMap(_.productRatePlans)
        MembershipCatalog.fromZuora(productFamily)(productRatePlans).fold(
          { errs => throw new RuntimeException(
            s"Failed creating the membership catalog for environment ${backendType.name}: ${errs.list.mkString(", ")}") },
          identity
        )
      }
    }

    val agent = Agent(Await.result(makeCatalog, 2.minutes))
    Akka.system.scheduler.schedule(0.milli, 5.minutes)(makeCatalog.map(agent.send))
    new CatalogService(agent, productFamily)
  }

}

class CatalogService(agent: Agent[MembershipCatalog],
                     val productFamily: Membership
                     ) extends commonapi.CatalogService {
  override def catalog: MembershipCatalog = agent.get
}
