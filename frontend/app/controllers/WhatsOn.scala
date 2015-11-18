package controllers

import com.github.nscala_time.time.Imports._
import com.gu.i18n.{Currency, GBP}
import configuration.CopyConfig
import model.RichEvent.MasterclassEvent._
import model.RichEvent._
import model._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Controller
import services._
import tracking.ActivityTracking

trait WhatsOn extends Controller with ActivityTracking {

  val guLiveEvents: EventbriteService
  val localEvents: EventbriteService
  val masterclassEvents: EventbriteService

  private def allEvents = {
    guLiveEvents.events ++ localEvents.events
  }

  private def allEventsByLocation(location: String) = {
    guLiveEvents.getEventsByLocation(location) ++ localEvents.getEventsByLocation(location)
  }

  private def allEventsInArchive = {
    guLiveEvents.getEventsArchive.toList.flatten ++ localEvents.getEventsArchive.toList.flatten
  }

  private def locationFilterItems = {
    getCitiesWithCount(allEvents).map(FilterItem.tupled)
  }

  def list = NoCacheAction.async { implicit request =>
    implicit val currency = countryGroup.currency
    val pageInfo = PageInfo(
      CopyConfig.copyTitleEvents,
      request.path,
      Some(CopyConfig.copyDescriptionEvents)
    )

    val locationOpt = request.getQueryString("location").filter(_.trim.nonEmpty)
    val featuredEvents = EventGroup("Featured", guLiveEvents.getFeaturedEvents)
    val events = EventGroup("What's on", chronologicalSort(locationOpt.fold(allEvents)(allEventsByLocation)))

    TouchpointBackend.Normal.catalog.map(cat =>
      Ok(views.html.event.eventsList(
        cat,
        pageInfo,
        events,
        featuredEvents,
        locationFilterItems,
        locationOpt
      ))
    )
  }

  def calendar = NoCacheAction { implicit request =>
    val locationOpt = request.getQueryString("location").filter(_.trim.nonEmpty)
    implicit val currency = countryGroup.currency

    val calendarEvents =
      CalendarMonthDayGroup("Calendar", groupEventsByDayAndMonth(locationOpt.fold(allEvents)(allEventsByLocation)))

    Ok(views.html.event.calendar(
      PageInfo(s"${calendarEvents.title} | Events", request.path, None),
      calendarEvents,
      locationFilterItems,
      locationOpt
    ))
  }

  def listArchive = NoCacheAction { implicit request =>
    val calendarArchive =
      CalendarMonthDayGroup("Archive", groupEventsByDayAndMonth(allEventsInArchive)(implicitly[Ordering[LocalDate]].reverse))

    implicit val currency = countryGroup.currency

    Ok(views.html.event.eventsListArchive(
      PageInfo(s"${calendarArchive.title} | Events", request.path, None),
      calendarArchive
    ))
  }

  def masterclassesList = NoCacheAction.async { implicit request =>
    implicit val currency = countryGroup.currency

    val pageInfo = PageInfo(
      CopyConfig.copyTitleMasterclasses,
      request.path,
      Some(CopyConfig.copyDescriptionMasterclasses)
    )
    val eventGroup = EventGroup("Masterclasses", masterclassEvents.events)

    TouchpointBackend.Normal.catalog.map(cat =>
      Ok(views.html.event.masterclassesList(cat, pageInfo, eventGroup)))
  }

  def masterclassesListFilteredBy(rawTag: String, rawSubTag: String = "") = NoCacheAction.async { implicit request =>
    implicit val currency = countryGroup.currency

    val pageInfo = PageInfo(
      CopyConfig.copyTitleMasterclasses,
      request.path,
      Some(CopyConfig.copyDescriptionMasterclasses)
    )
    val tag = decodeTag( if(rawSubTag.nonEmpty) rawSubTag else rawTag )
    val eventGroup = EventGroup("Masterclasses", masterclassEvents.getTaggedEvents(tag))

    TouchpointBackend.Normal.catalog.map { cat =>
      Ok(views.html.event.masterclassesList(cat, pageInfo, eventGroup, decodeTag(rawTag), decodeTag(rawSubTag)))
    }
  }
}

object WhatsOn extends WhatsOn {
  val guLiveEvents = GuardianLiveEventService
  val localEvents = LocalEventService
  val masterclassEvents = MasterclassEventService
}
