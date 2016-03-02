package services

import akka.agent.Agent
import com.gu.memsub.util.WebServiceHelper
import com.gu.monitoring.StatusMetrics
import com.netaporter.uri.Uri
import com.squareup.okhttp.Request
import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import model.Grid._
import model.GridDeserializer._
import model.RichEvent.GridImage
import monitoring.GridApiMetrics
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GridService extends WebServiceHelper[GridObject, Error] with LazyLogging{

  val gridUrl: String = "https://media.gutools.co.uk/images/"
  val CropQueryParam = "crop"
  def cropParam(url: Uri) = url.query.param(CropQueryParam)

  case class ImageIdWithCrop(id: String, crop: String)
  object ImageIdWithCrop {
    implicit val writesImageIdWithCrop = Json.writes[ImageIdWithCrop]


    def fromGuToolsUri(uri: Uri): Option[ImageIdWithCrop] =
      for {
        imageId <- uri.path.split("/").lastOption
        crop <-  uri.query.param(CropQueryParam)
        if uri.toString().startsWith(gridUrl)
      } yield ImageIdWithCrop(imageId, crop)
  }

  lazy val agent = Agent[Map[ImageIdWithCrop, GridImage]](Map.empty)

  def getRequestedCrop(gridId: ImageIdWithCrop) : Future[Option[GridImage]] = {
    val currentImageData = agent.get()
    if(currentImageData.contains(gridId)) Future.successful(currentImageData.get(gridId))
    else {
      getGrid(gridId).map { grid =>
        for {
          exports <- grid.data.exports
          assets = findAssets(exports, gridId.crop)
          if assets.nonEmpty
        } yield {
          val image = GridImage(assets, grid.data.metadata)
          agent send {
            oldImageData =>
              val newImageData = oldImageData + (gridId -> image)
              logger.trace(s"Adding image $gridId to the event image map")
              newImageData
          }
          image
        }
      }
    }
  }.recover { case e =>
    logger.error(s"Error getting crop for $gridId", e)
    None
  } // We should return no image, rather than die

  def getGrid(gridId: ImageIdWithCrop): Future[GridResult] =
    get[GridResult](gridId.id, CropQueryParam -> gridId.crop)

  def findAssets(exports: List[Export], cropId: String) = {
    val requestedExport = exports.find(_.id == cropId)
    requestedExport.map(_.assets).getOrElse(Nil)
  }

  override val wsUrl: String = Config.gridConfig.apiUrl

  override def wsPreExecute(req: Request.Builder): Request.Builder = req.addHeader("X-Gu-Media-Key", Config.gridConfig.key)

  override val wsMetrics: StatusMetrics = GridApiMetrics
}

case class GridConfig(apiUrl: String, key: String)

