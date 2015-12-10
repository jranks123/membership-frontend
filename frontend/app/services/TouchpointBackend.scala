package services

import com.gu.identity.play.IdMinimalUser
import com.gu.membership.salesforce.Contact._
import com.gu.membership.salesforce.ContactDeserializer.Keys
import com.gu.membership.salesforce._
import com.gu.membership.stripe.{Stripe, StripeService}
import com.gu.membership.touchpoint.TouchpointBackendConfig
import com.gu.membership.zuora
import com.gu.membership.zuora.soap.ClientWithFeatureSupplier
import com.gu.membership.zuora.{rest, soap}
import com.gu.monitoring.{ServiceMetrics, StatusMetrics}
import com.gu.services.ZuoraPaymentService
import com.netaporter.uri.Uri
import configuration.Config
import model.{FeatureChoice, MembershipCatalog}
import monitoring.TouchpointBackendMetrics
import play.api.Play.current
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.libs.Akka
import tracking._
import utils.TestUsers.isTestUser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
    }, WS.client)

    val restBackendConfig = backend.zuoraRest.copy(url = Uri.parse(backend.zuoraRestUrl(Config.config)))

    val zuoraSoapClient = new ClientWithFeatureSupplier(FeatureChoice.codes, backend.zuoraSoap, backend.zuoraMetrics("zuora-soap-client"), Akka.system())
    val zuoraRestClient = new rest.Client(restBackendConfig, backend.zuoraMetrics("zuora-rest-client"))
    val productFamily = Config.productFamily(restBackendConfig.envName)
    val zSubscriptionService = new zuora.SubscriptionService(zuoraSoapClient, zuoraRestClient, stripeService)
    val paymentService = new ZuoraPaymentService(stripeService, zSubscriptionService)
    val subscriptionService = new SubscriptionService(zuoraSoapClient, zuoraRestClient, backend.zuoraMetrics("zuora-rest-client"), productFamily, backendType, paymentService)
    val memberRepository = new FrontendMemberRepository(backend.salesforce)

    TouchpointBackend(memberRepository, stripeService, zuoraSoapClient, zuoraRestClient, subscriptionService)
  }

  val Normal = TouchpointBackend(BackendType.Default)
  val TestUser = TouchpointBackend(BackendType.Testing)

  val All = Seq(Normal, TestUser)

  def forUser(user: IdMinimalUser): TouchpointBackend = if (isTestUser(user)) TestUser else Normal
  // Convenience method for Salesforce users. Assumes firstName matches the test user key generated by the app
  def forUser(user: Contact[MemberStatus, PaymentMethod]): TouchpointBackend = if (isTestUser(user)) TestUser else Normal
}

case class TouchpointBackend(memberRepository: FrontendMemberRepository,
                             stripeService: StripeService,
			                       zuoraSoapClient: soap.ClientWithFeatureSupplier,
			                       zuoraRestClient: rest.Client,
			                       subscriptionService: SubscriptionService
			                       ) extends ActivityTracking {

  def catalog: Future[MembershipCatalog] = subscriptionService.membershipCatalog.get()

  //TODO: consider moving the following methods elsewhere
  def updateDefaultCard(member: Contact[Member, StripePayment], token: String): Future[Stripe.Card] = {
    for {
      customer <- stripeService.Customer.updateCard(member.stripeCustomerId, token)
      memberId <- memberRepository.upsert(member.identityId, Json.obj(Keys.DEFAULT_CARD_ID -> customer.card.id))
    } yield customer.card
  }
}
