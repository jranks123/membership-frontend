package controllers
import com.softwaremill.macwire._

trait ControllerModule {
  lazy val cachedAssets = wire[CachedAssets]
  lazy val cacheBustedAssets = wire[CacheBustedAssets]
  lazy val frontpage = wire[FrontPage]
  lazy val healthcheck = wire[Healthcheck]
  lazy val testing = wire[Testing]
  lazy val joiner = wire[Joiner]
  lazy val login = wire[Login]
  lazy val staffAuth = wire[StaffAuth]
  lazy val staff = wire[Staff]
  lazy val OAuth = wire[OAuth]
  lazy val subscription = wire[Subscription]
  lazy val whatsOn = wire[WhatsOn]
  lazy val event = wire[Event]
  lazy val tier = wire[TierController]
  lazy val info = wire[Info]
  lazy val patternLibrary = wire[PatternLibrary]
  lazy val user = wire[User]
  lazy val redirects = wire[Redirects]
  lazy val vanityUrl = wire[VanityUrl]
}
