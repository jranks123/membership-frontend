package services.api

import com.gu.identity.play.{IdMinimalUser, IdUser}
import com.gu.membership.salesforce.{ContactId, Tier}
import com.gu.membership.stripe.Stripe.Customer
import forms.MemberForm.JoinForm
import model.{SFMember, GenericSFContact}
import monitoring.MemberMetrics
import services.FrontendMemberRepository.UserId

import scala.concurrent.Future

trait SalesforceService {
  def getMember(userId: UserId): Future[Option[SFMember]]
  def metrics: MemberMetrics
  def upsert(user: IdUser, userData: JoinForm): Future[ContactId]
  def updateMemberStatus(user: IdMinimalUser, tier: Tier, customer: Option[Customer]): Future[ContactId]
  def updateCardId(identityId: String, cardId: String): Future[ContactId]
  def getStripeCustomer(contact: GenericSFContact): Future[Option[Customer]]
}