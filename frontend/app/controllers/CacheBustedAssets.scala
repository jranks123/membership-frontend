package controllers

import play.api.mvc.{AnyContent, Action, Controller}
import views.support.Asset

class CacheBustedAssets extends Controller {
  def at(path: String): Action[AnyContent] =  {
    println("")
    controllers.Assets.at("/public", Asset.map(path), true)
  }
}
