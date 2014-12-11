import play.api.mvc.ActionBuilder
import play.api.mvc.Security.AuthenticatedRequest
import shapeless.HList

package actions

import actions.Fallbacks._
import actions.Functions._
import com.gu.googleauth
import configuration.Config
import controllers._
import play.api.http.HeaderNames._
import play.api.mvc.{DiscardingCookie, ActionBuilder}
import play.api.mvc.Results._

trait CommonActions {

  val NoCacheAction = resultModifier(NoCache(_))

  val CachedAction = resultModifier(Cached(_))

  val BlankAuth: ActionBuilder[({type λ[A] = AuthenticatedRequest[A, HList]})#λ] = NoCacheAction andThen initialAccumulatingAuthenticator

  val Cors = resultModifier(_.withHeaders(
    ACCESS_CONTROL_ALLOW_ORIGIN -> Config.corsAllowOrigin,
    ACCESS_CONTROL_ALLOW_CREDENTIALS -> "true"))

  val AuthenticatedAction = BlankAuth andThen(new AccumulatingAuthenticator(authenticated()))

  val AuthenticatedNonMemberAction = AuthenticatedAction andThen onlyNonMemberFilter()

  val GoogleAuthAction: ActionBuilder[GoogleAuthRequest] = OAuthActions.AuthAction

  val GoogleAuthenticatedStaffAction = BlankAuth andThen GoogleAuthAction

  val permanentStaffGroups = Config.staffAuthorisedEmailGroups

  val PermanentStaffNonMemberAction =
    GoogleAuthenticatedStaffAction andThen
    isInAuthorisedGroupGoogle[googleauth.UserIdentity, GoogleAuthRequest](permanentStaffGroups, views.html.fragments.oauth.staffUnauthorisedError())

  val AuthenticatedStaffNonMemberAction =
    AuthenticatedAction andThen
    onlyNonMemberFilter() andThen
    googleAuthenticationRefiner() andThen
    isInAuthorisedGroupGoogle[IdAndGoogle, IdAndGoogleAuthRequest](permanentStaffGroups, views.html.fragments.oauth.staffUnauthorisedError())

  val MemberAction = AuthenticatedAction andThen memberRefiner()

  val StaffMemberAction = AuthenticatedAction andThen memberRefiner(onNonMember = joinStaffMembership(_))

  val PaidMemberAction = MemberAction andThen paidMemberRefiner()

  val AjaxAuthenticatedAction = Cors andThen NoCacheAction andThen authenticated(onUnauthenticated = _ => forbidAndDropGuMemCookie)

  val AjaxMemberAction = AjaxAuthenticatedAction andThen memberRefiner(onNonMember = _ => forbidAndDropGuMemCookie)

  val AjaxPaidMemberAction = AjaxMemberAction andThen paidMemberRefiner(onFreeMember = _ => Forbidden)
  
  def forbidAndDropGuMemCookie = Forbidden.discardingCookies(DiscardingCookie("GU_MEM"))
}


trait OAuthActions extends googleauth.Actions {
  val authConfig = Config.googleAuthConfig

  val loginTarget = routes.OAuth.loginAction()
}

object OAuthActions extends OAuthActions
