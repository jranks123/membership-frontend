package controllers

import com.typesafe.scalalogging.LazyLogging
import play.api.mvc.{Controller, Cookie}
import play.twirl.api.Html
import utils.TestUsers.testUsers
import actions.Functions._

class Testing extends Controller with LazyLogging {


  /**
   * Make sure to use cannonical @guardian.co.uk for addresses
   */
  val AuthorisedTester = GoogleAuthenticatedStaffAction andThen isInAuthorisedGroupGoogleAuthReq(
    Set(
      "membership.dev@guardian.co.uk",
      "dig.dev.web-engineers@guardian.co.uk",
      "membership.testusers@guardian.co.uk",
      "touchpoint@guardian.co.uk",
      "crm@guardian.co.uk"
    ),
    views.html.fragments.oauth.staffWrongGroup()
  )

  def testUser = AuthorisedTester { implicit request =>
    val testUserString = testUsers.generate()
    logger.info(s"Generated test user string $testUserString for ${request.user.email}")
    Ok(views.html.testing.testUsers(testUserString)).withCookies(Testing.analyticsOffCookie)
  }

  def analyticsOff = CachedAction {
    Ok(s"${Testing.analyticsOffCookie.name} cookie dropped").withCookies(Testing.analyticsOffCookie)
  }

}

object Testing {
  val AnalyticsCookieName = "ANALYTICS_OFF_KEY"

  val analyticsOffCookie = Cookie(AnalyticsCookieName, "true", httpOnly = false)

}
