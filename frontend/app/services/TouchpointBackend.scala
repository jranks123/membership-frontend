package services

import com.gu.identity.play.IdMinimalUser
import com.gu.membership.model.FriendTierPlan
import com.gu.membership.salesforce.Member.Keys
import com.gu.membership.salesforce._
import com.gu.membership.stripe.{Stripe, StripeService}
import com.gu.membership.touchpoint.TouchpointBackendConfig
import com.gu.membership.zuora.soap.ClientWithFeatureSupplier
import com.gu.membership.zuora.{rest, soap}
import com.gu.monitoring.{ServiceMetrics, StatusMetrics}
import com.netaporter.uri.dsl._
import configuration.Config
import model.FeatureChoice
import monitoring.TouchpointBackendMetrics
import play.api.libs.json.Json
import play.libs.Akka
import tracking._
import utils.TestUsers.isTestUser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TouchpointBackend {

  import TouchpointBackendConfig.BackendType

  implicit class TouchpointBackendConfigLike(tpbc: TouchpointBackendConfig) {
    def zuoraEnvName: String = tpbc.zuora.envName
    def zuoraMetrics(component: String): ServiceMetrics = new ServiceMetrics(zuoraEnvName, "membership", component)
    def zuoraRestUrl(config: com.typesafe.config.Config): String =
      config.getString(s"touchpoint.backend.environments.$zuoraEnvName.zuora.api.restUrl")
  }

  def apply(backendType: TouchpointBackendConfig.BackendType): TouchpointBackend = {
    val touchpointBackendConfig = TouchpointBackendConfig.byType(backendType, Config.config)
    TouchpointBackend(touchpointBackendConfig)
  }

  def apply(backend: TouchpointBackendConfig): TouchpointBackend = {
    val stripeService = new StripeService(backend.stripe, new TouchpointBackendMetrics with StatusMetrics {
      val backendEnv = backend.stripe.envName
      val service = "Stripe"
    })

    val restBackendConfig = backend.zuora.copy(url = backend.zuoraRestUrl(Config.config))

    val zuoraSoapClient = new ClientWithFeatureSupplier(FeatureChoice.codes, backend.zuora, backend.zuoraMetrics("zuora-soap-client"), Akka.system())
    val zuoraRestClient = new rest.Client(restBackendConfig, backend.zuoraMetrics("zuora-rest-client"))
    val subscriptionService = new SubscriptionService(zuoraSoapClient, zuoraRestClient, backend.zuoraMetrics("zuora-rest-client"))
    val memberRepository = new FrontendMemberRepository(backend.salesforce)

    TouchpointBackend(memberRepository, stripeService, zuoraSoapClient, zuoraRestClient, subscriptionService)
  }

  val Normal = TouchpointBackend(BackendType.Default)
  val TestUser = TouchpointBackend(BackendType.Testing)

  val All = Seq(Normal, TestUser)

  def forUser(user: IdMinimalUser): TouchpointBackend = if (isTestUser(user)) TestUser else Normal
  // Convenience method for Salesforce users. Assumes firstName matches the test user key generated by the app
  def forUser(user: Member): TouchpointBackend = if (isTestUser(user)) TestUser else Normal
}

case class TouchpointBackend(memberRepository: FrontendMemberRepository,
                             stripeService: StripeService,
                             zuoraSoapClient: soap.ClientWithFeatureSupplier,
                             zuoraRestClient: rest.Client,
                             subscriptionService: SubscriptionService) extends ActivityTracking {

  def updateDefaultCard(member: PaidMember, token: String): Future[Stripe.Card] = {
    for {
      customer <- stripeService.Customer.updateCard(member.stripeCustomerId, token)
      memberId <- memberRepository.upsert(member.identityId, Json.obj(Keys.DEFAULT_CARD_ID -> customer.card.id))
    } yield customer.card
  }

  def cancelSubscription(member: Member, user: IdMinimalUser, campaignCode: Option[String] = None): Future[String] = {
    for {
      subscription <- subscriptionService.cancelSubscription(member, member.tier == Tier.Friend)
    } yield {
      memberRepository.metrics.putCancel(member.tier)
      track(MemberActivity("cancelMembership", MemberData(member.salesforceContactId, member.identityId, member.tier.name, campaignCode = campaignCode)), user)
      ""
    }
  }

  def downgradeSubscription(member: Member, user: IdMinimalUser, campaignCode: Option[String] = None): Future[String] = {
    for {
      _ <- subscriptionService.downgradeSubscription(member, FriendTierPlan)
    } yield {
      memberRepository.metrics.putDowngrade(member.tier)
      track(
        MemberActivity(
          "downgradeMembership",
          MemberData(
            member.salesforceContactId,
            member.identityId,
            member.tier.name,
            Some(DowngradeAmendment(member.tier)), //getting effective date and subscription annual / month is proving difficult
            campaignCode = campaignCode
          )),
        user)

      ""
    }
  }

}
