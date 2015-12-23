package services

import com.gu.identity.play.IdMinimalUser
import com.gu.membership.MembershipCatalog
import com.gu.membership.services.{api => commonapi}
import com.gu.memsub
import com.gu.memsub.services.PaymentService
import com.gu.monitoring.{ServiceMetrics, StatusMetrics}
import com.gu.salesforce._
import com.gu.stripe.StripeService
import com.gu.touchpoint.TouchpointBackendConfig
import com.gu.zuora.api.ZuoraService
import com.gu.zuora.soap.ClientWithFeatureSupplier
import com.gu.zuora.{ZuoraService => ZuoraServiceImpl, rest, soap}
import com.netaporter.uri.Uri
import configuration.Config
import model.FeatureChoice
import monitoring.TouchpointBackendMetrics
import play.libs.Akka
import tracking._
import utils.TestUsers.isTestUser

object TouchpointBackend {

  import TouchpointBackendConfig.BackendType

  implicit class TouchpointBackendConfigLike(tpbc: TouchpointBackendConfig) {
    def zuoraEnvName: String = tpbc.zuoraSoap.envName
    def zuoraMetrics(component: String): ServiceMetrics = new ServiceMetrics(zuoraEnvName, "membership", component)
    def zuoraRestUrl(config: com.typesafe.config.Config): String =
      config.getString(s"touchpoint.backend.environments.$zuoraEnvName.zuora.api.restUrl")
  }

  def apply(backendType: TouchpointBackendConfig.BackendType): TouchpointBackend = {
    val touchpointBackendConfig = TouchpointBackendConfig.byType(backendType, Config.config)
    TouchpointBackend(touchpointBackendConfig, backendType)
  }

  def apply(backend: TouchpointBackendConfig, backendType: BackendType): TouchpointBackend = {
    val stripeService = new StripeService(backend.stripe, new TouchpointBackendMetrics with StatusMetrics {
      val backendEnv = backend.stripe.envName
      val service = "Stripe"
    })

    val restBackendConfig = backend.zuoraRest.copy(url = Uri.parse(backend.zuoraRestUrl(Config.config)))
    implicit val _bt = backendType

    val zuoraSoapClient = new ClientWithFeatureSupplier(FeatureChoice.codes, backend.zuoraSoap, backend.zuoraMetrics("zuora-soap-client"), Akka.system())
    val zuoraRestClient = new rest.Client(restBackendConfig, backend.zuoraMetrics("zuora-rest-client"))
    val productFamily = Config.productFamily(restBackendConfig.envName)
    val zuoraService = new ZuoraServiceImpl(zuoraSoapClient, zuoraRestClient, productFamily)
    val catalogService = CatalogService(zuoraRestClient, productFamily)
    val subscriptionService = new memsub.services.SubscriptionService(zuoraService, stripeService, catalogService)
    val paymentService = new PaymentService(stripeService, subscriptionService, zuoraService, catalogService)
    val salesforceService = new SalesforceService(backend.salesforce)
    val identityService = IdentityService(IdentityApi)
    val memberService = new MemberService(identityService, salesforceService, zuoraService, stripeService, subscriptionService, catalogService, paymentService)

    TouchpointBackend(salesforceService, stripeService, zuoraSoapClient, zuoraRestClient, memberService, catalogService, zuoraService)
  }

  val Normal = TouchpointBackend(BackendType.Default)
  val TestUser = TouchpointBackend(BackendType.Testing)

  val All = Seq(Normal, TestUser)

  def forUser(user: IdMinimalUser): TouchpointBackend = if (isTestUser(user)) TestUser else Normal
  // Convenience method for Salesforce users. Assumes firstName matches the test user key generated by the app
  def forUser(user: Contact[MemberStatus, PaymentMethod]): TouchpointBackend = if (isTestUser(user)) TestUser else Normal
}

case class TouchpointBackend(salesforceService: api.SalesforceService,
                             stripeService: StripeService,
                             zuoraSoapClient: soap.ClientWithFeatureSupplier,
                             zuoraRestClient: rest.Client,
                             memberService: api.MemberService,
                             catalogService: commonapi.CatalogService,
                             zuoraService: ZuoraService) extends ActivityTracking {

  def catalog: MembershipCatalog = catalogService.catalog
}
