import com.gu.googleauth
import com.gu.identity.play.AuthenticatedIdUser
import com.gu.memsub.Subscriber.{Member, FreeMember, PaidMember}
import com.gu.salesforce._
import com.gu.memsub.util.Timing
import monitoring.MemberAuthenticationMetrics
import play.api.mvc.Security.AuthenticatedRequest
import play.api.mvc.{Cookie, WrappedRequest}
import services._

import scala.concurrent.{ExecutionContext, Future}


package object actions {

  type AuthRequest[A] = AuthenticatedRequest[A, AuthenticatedIdUser]
  type GoogleAuthRequest[A] = AuthenticatedRequest[A, googleauth.UserIdentity]

  trait BackendProvider {
    def touchpointBackend: TouchpointBackend
  }

  implicit class RichAuthRequest[A](req: AuthRequest[A]) extends BackendProvider {
    lazy val touchpointBackend = TouchpointBackend.forUser(req.user)

    def forMemberOpt[T](f: Option[Contact] => T)(implicit executor: ExecutionContext): Future[T] =
      Timing.record(MemberAuthenticationMetrics, s"${req.method} ${req.path}") {
        for {
          memberOpt <- touchpointBackend.salesforceService.getMember(req.user.id)
        } yield f(memberOpt)
      }
    }

  case class SubscriptionRequest[A](touchpointBackend: TouchpointBackend,
                                    request: AuthRequest[A]) extends AuthRequest(request.user, request) with BackendProvider {

    def idCookies: Option[Seq[Cookie]] = for {
      guu <- request.cookies.get("GU_U")
      scguu <- request.cookies.get("SC_GU_U")
    } yield Seq(guu, scguu)

    def this(other: SubscriptionRequest[A]) =
      this(other.touchpointBackend, other.request)
  }

  trait Subscriber {
    def subscriber: Member
  }

  trait PaidSubscriber extends Subscriber {
    def subscriber: PaidMember
  }

  trait FreeSubscriber extends Subscriber {
    def subscriber: FreeMember
  }

  case class IdentityGoogleAuthRequest[A](googleUser: googleauth.UserIdentity, request: AuthRequest[A]) extends WrappedRequest[A](request) with BackendProvider {
    lazy val touchpointBackend = TouchpointBackend.forUser(request.user)
    val identityUser = request.user
  }
}
