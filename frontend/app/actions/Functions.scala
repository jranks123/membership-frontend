package actions

import actions.Fallbacks._
import com.gu.googleauth
import com.gu.googleauth.{GoogleGroupChecker, UserIdentity}
import com.gu.membership.salesforce.PaidMember
import com.gu.membership.util.Timing
import com.gu.monitoring.CloudWatch
import com.typesafe.scalalogging.slf4j.LazyLogging
import configuration.Config
import controllers.IdentityRequest
import model.IdMinimalUser
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Security.{AuthenticatedRequest, AuthenticatedBuilder}
import play.api.mvc._
import play.twirl.api.Html
import services.{AuthenticationService, IdentityService}
import shapeless._
import shapeless.ops.hlist.Selector
import services.{IdentityApi, AuthenticationService, IdentityService}

import scala.concurrent.Future

/**
 * These ActionFunctions serve as components that can be composed to build the
 * larger, more-generally useful pipelines in 'CommonActions'.
 *
 * https://www.playframework.com/documentation/2.3.x/ScalaActionsComposition
 */
object Functions extends LazyLogging {

//  val initialAccumlatingAuthenticator = new ActionBuilder[AuthenticatedRequest[_, HNil]] {
//    override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = block(new AuthenticatedRequest[A, HNil](HNil, request))
//  }
//

  // def authenticated(onUnauthenticated: RequestHeader => Result = chooseSigninOrRegister(_)): ActionBuilder[AuthRequest] =


  val initialAccumulatingAuthenticator = new AuthenticatedBuilder[HList](_ => Some(HNil))

  //def authAccum[U, UList <: HList](authenticatedBuilder: AuthenticatedBuilder[U]): ActionFunction[]

  class AccumulatingAuthenticator[U, UList <: HList](authenticatedBuilder: AuthenticatedBuilder[U])
    extends ActionFunction[({ type R[A] = AuthenticatedRequest[A, UList] })#R, ({ type P[A] = AuthenticatedRequest[A, U :: UList] })#P] {

    def invokeBlock[A](request: AuthenticatedRequest[A, UList], block: AuthenticatedRequest[A, U :: UList] => Future[Result]) = {
      val adaptingBlock: (AuthenticatedRequest[A, U]) => Future[Result] = {
        monoAuthReq => block(new AuthenticatedRequest[A, U :: UList](monoAuthReq.user :: request.user, request))
      }
      authenticatedBuilder.authenticate(request, adaptingBlock)
    }
  }

  val googleGroupChecker = new GoogleGroupChecker(Config.googleGroupCheckerAuthConfig)

  def resultModifier(f: Result => Result) = new ActionBuilder[Request] {
    def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = block(request).map(f)
  }

  def authenticated(onUnauthenticated: RequestHeader => Result = chooseSigninOrRegister(_)) =
    new AuthenticatedBuilder(AuthenticationService.authenticatedUserFor(_), onUnauthenticated)


  def memberRefiner(onNonMember: RequestHeader => Result = notYetAMemberOn(_)) =
    new ActionRefiner[AuthRequest, AnyMemberTierRequest] {
      override def refine[A](request: AuthRequest[A]) = request.forMemberOpt {
        _.map(member => MemberRequest(member, request)).toRight(onNonMember(request))
      }
    }

  def onlyNonMemberFilter(onPaidMember: RequestHeader => Result = changeTier(_)) = new ActionFilter[AuthRequest] {
    override def filter[A](request: AuthRequest[A]) = request.forMemberOpt(_.map(_ => onPaidMember(request)))
  }


  def isInAuthorisedGroupGoogleFoo[UList <: HList, R[_] <: AuthenticatedRequest[_,UList]]
  (includedGroups: Set[String],errorWhenNotInAcceptedGroups: Html)(implicit g: Selector[UList, googleauth.UserIdentity]) = new ActionFilter[R] {
    override def filter[A](request: R[A]) = {
      val email = request.user.select[googleauth.UserIdentity].email
      for (usersGroups <- googleGroupChecker.retrieveGroupsFor(email)) yield {
        if (includedGroups.intersect(usersGroups).nonEmpty) None else {
          logger.info(s"Excluding $email from '${request.path}' - not in accepted groups: $includedGroups")
          Some(unauthorisedStaff(errorWhenNotInAcceptedGroups)(request))
        }
      }
    }
  }

  def paidMemberRefiner(onFreeMember: RequestHeader => Result = changeTier(_)) =
    new ActionRefiner[AnyMemberTierRequest, PaidMemberRequest] {
      override def refine[A](request: AnyMemberTierRequest[A]) = Future.successful {
        request.member match {
          case paidMember: PaidMember => Right(MemberRequest(paidMember, request.request))
          case _ => Left(onFreeMember(request))
        }
      }
    }

  def googleAuthenticationRefiner(onNonAuthentication: RequestHeader => Result = OAuthActions.sendForAuth) = {
    new ActionRefiner[AuthRequest, IdAndGoogleAuthRequest] {
      override def refine[A](request: AuthRequest[A]) = Future.successful {
        //Copy the private helper method in play-googleauth to ensure the user is Google auth'd
        //see https://github.com/guardian/play-googleauth/blob/master/module/src/main/scala/com/gu/googleauth/actions.scala#L59-60
        val userIdentityOpt = UserIdentity.fromRequest(request).filter(_.isValid || !OAuthActions.authConfig.enforceValidity).map(googleUser => new IdAndGoogleAuthRequest(IdAndGoogle(request.user, googleUser), request))
        userIdentityOpt.toRight(onNonAuthentication(request))
      }
    }
  }

  def matchingGuardianEmail[UList <: HList, R[_] <: AuthenticatedRequest[_,UList]](onNonGuEmail: RequestHeader => Result = joinStaffMembership(_).flashing("error" -> "Identity email must match Guardian email"))(implicit u: Selector[UList, IdMinimalUser], g: Selector[UList, googleauth.UserIdentity]) = new ActionFilter[IdentityGoogleAuthRequest] {
    override def filter[A](request: IdentityGoogleAuthRequest[A]) = {
      for {
        user <- IdentityService(IdentityApi).getFullUserDetails(request.identityUser, IdentityRequest(request))
      } yield {
        if (GuardianDomains.emailsMatch(request.googleUser.email, user.primaryEmailAddress)) None
        else Some(onNonGuEmail(request))
      }
    }
  }

  def metricRecord(cloudWatch: CloudWatch, metricName: String) = new ActionBuilder[Request] {
    def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) =
      Timing.record(cloudWatch, metricName) {
        block(request)
      }
  }
}
