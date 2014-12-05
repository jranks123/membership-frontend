import com.gu.googleauth
import com.gu.membership.salesforce.{Member, PaidMember, Tier}
import com.gu.membership.util.Timing
import model.IdMinimalUser
import play.api.Logger
import play.api.mvc.Security.AuthenticatedRequest
import play.api.mvc.WrappedRequest
import services._

import scala.concurrent.{ExecutionContext, Future}

package object actions {

  val logger = Logger(this.getClass())

  case class IdAndGoogle(idMinimal: IdMinimalUser, google: googleauth.UserIdentity)

  type AuthRequest[A] = AuthenticatedRequest[A, IdMinimalUser]

  type GoogleAuthRequest[A] = AuthenticatedRequest[A, googleauth.UserIdentity]

  type IdAndGoogleAuthRequest[A] = AuthenticatedRequest[A, IdAndGoogle]

  trait GoogleAuthed[U] {
    def googleUserIn(u: U): googleauth.UserIdentity
  }

  implicit val gConv  = new GoogleAuthed[googleauth.UserIdentity] { def googleUserIn(u: googleauth.UserIdentity) = u }
  implicit val igConv = new GoogleAuthed[IdAndGoogle] { def googleUserIn(u: IdAndGoogle) = u.google }

  implicit class RichAuthRequest[A](req: AuthRequest[A]) {
    lazy val touchpointBackend = TouchpointBackend.forUser(req.user)

    def forMemberOpt[A, T](f: Option[Member] => T)(implicit executor: ExecutionContext): Future[T] =
      Timing.record(MemberAuthenticationMetrics, s"${req.method} ${req.path}") {
        for {
          memberOpt <- touchpointBackend.memberRepository.get(req.user.id).map(_.filter(_.tier > Tier.None))
        } yield f(memberOpt)
      }
    }

  case class MemberRequest[A, +M <: Member](val member: M, request: AuthRequest[A]) extends WrappedRequest[A](request) {
    val user = request.user

    lazy val touchpointBackend = request.touchpointBackend
  }

  type AnyMemberTierRequest[A] = MemberRequest[A, Member]

  type PaidMemberRequest[A] = MemberRequest[A, PaidMember]

}
