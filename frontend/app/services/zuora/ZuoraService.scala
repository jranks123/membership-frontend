package services.zuora

import model.Zuora._
import model.ZuoraDeserializer._
import model.ZuoraReaders.ZuoraReader
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import play.api.Logger
import play.api.libs.ws.WS
import utils.ScheduledTask

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.xml.PrettyPrinter
import play.api.Play.current

case class ZuoraServiceError(s: String) extends Throwable {
  override def getMessage: String = s
}

object ZuoraServiceHelpers {
  def formatDateTime(dt: DateTime): String = {
    val str = ISODateTimeFormat.dateTime().print(dt.withZone(DateTimeZone.UTC))
    // Zuora doesn't accept Z for timezone
    str.replace("Z", "+00:00")
  }
}

case class ZuoraApiConfig(url: String, username: String, password: String)

class ZuoraService(apiConfig: ZuoraApiConfig) extends ScheduledTask[Authentication] {

  val initialValue = Authentication("", "")
  val initialDelay = 0.seconds
  val interval = 2.hours

  def refresh() = mkRequest(Login(apiConfig))

  implicit def authentication: Authentication = agent.get()

  def mkRequest[T <: ZuoraObject](action: ZuoraAction[T])(implicit reader: ZuoraReader[T]): Future[T] = {
    val url = if (action.authRequired) authentication.url else apiConfig.url

    if (action.authRequired && authentication.url.length == 0) {
      throw ZuoraServiceError(s"Can't build authenticated request for ${getClass.getSimpleName}, no Zuora authentication")
    }

    Logger.debug(s"Zuora action ${getClass.getSimpleName}")

    WS.url(url).post(action.xml).map { result =>
      Logger.debug(s"Got result ${result.status}")
      Logger.debug(new PrettyPrinter(70, 2).format(result.xml))

      reader.read(result.xml) match {
        case Left(error) => throw error
        case Right(obj) => obj
      }
    }
  }

  def query(fields: Seq[String], table: String, where: String): Future[Seq[Map[String, String]]] = {
    val q = s"SELECT ${fields.mkString(",")} FROM $table WHERE $where"
    mkRequest(Query(q)).map { case QueryResult(results) => results }
  }

  def queryOne(fields: Seq[String], table: String, where: String): Future[Map[String, String]] = {
    query(fields, table, where).map { results =>
      if (results.length != 1) {
        throw new ZuoraServiceError(s"Query '$fields $table $where' returned ${results.length} results, expected one")
      }

      results(0)
    }
  }

  def queryOne(field: String, table: String, where: String): Future[String] =
    queryOne(Seq(field), table, where).map(_(field))

}