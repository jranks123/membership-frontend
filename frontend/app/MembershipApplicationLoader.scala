import controllers._
import play.api.{BuiltInComponents, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api._
import play.api.routing.Router
import com.softwaremill.macwire._
import play.filters.csrf.CSRFComponents
import play.inject.Injector
import router.Routes

class MembershipApplicationLoader extends ApplicationLoader {

  override def load(context: Context) = {
    Logger.configure(context.environment)
    (new BuiltInComponentsFromContext(context) with MembershipComponents).application
  }
}

trait MembershipComponents extends BuiltInComponents with ControllerModule with CSRFComponents {
  lazy val prefix = "/"
  override val router: Router = wire[Routes]
  lazy val assets: Assets = wire[Assets]

}

