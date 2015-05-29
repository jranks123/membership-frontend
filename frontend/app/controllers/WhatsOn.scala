package controllers

import com.github.nscala_time.time.Imports._
import configuration.CopyConfig
import model.RichEvent.RichEvent
import model.{EventGroup, ContentItem, EventCollections, PageInfo, EventFilterLink, PeriodGroup}
import org.joda.time.{Interval, Period, DateTime}
import play.api.mvc.Controller
import services._
import tracking.ActivityTracking

import scala.collection.immutable.SortedMap

trait WhatsOn extends Controller with ActivityTracking {

  val guLiveEvents: EventbriteService
  val localEvents: EventbriteService
  val masterclassEvents: EventbriteService

  private def collectAllEvents = {
    guLiveEvents.getEvents ++ localEvents.getEvents ++ masterclassEvents.getEvents
  }

  private def groupEventsByDay(events: Seq[RichEvent]): SortedMap[LocalDate, Seq[RichEvent]] = {
    val unsortedMap = events.groupBy(_.start.toLocalDate)
    SortedMap(unsortedMap.toSeq :_*)
  }

  private def groupEventsByDayAndMonth(events: Seq[RichEvent]):  SortedMap[LocalDate, SortedMap[LocalDate, Seq[RichEvent]]] = {
    val unsortedMap = groupEventsByDay(events).groupBy(_._1.withDayOfMonth(1))
    SortedMap(unsortedMap.toSeq :_*)
  }

  private def groupEventsByMonth(events: Seq[RichEvent]): SortedMap[LocalDate, Seq[RichEvent]] = {
    val unsortedMap = events.groupBy(_.start.toLocalDate.withDayOfMonth(1)).map {
      case (monthDate, eventsForMonth) => monthDate -> eventsForMonth
    }
    SortedMap(unsortedMap.toSeq :_*)
  }

  def overview = GoogleAuthenticatedStaffAction { implicit request =>

    val pageInfo = PageInfo(
      CopyConfig.copyTitleEvents,
      request.path,
      Some(CopyConfig.copyDescriptionEvents)
    )

    val now = DateTime.now

    val events = EventCollections(
      // unstruct_event_1.eventSource:"viewEventDetails"
      trending=guLiveEvents.getEventsByIds(List(
        "15926171608",
        "16351430569",
        "15351696337",
        "15756402825",
        "16253355223",
        "16234171845",
        "16349419554",
        "15597340064",
        "16253236869",
        "16430264363"
      )),
      // unstruct_event_1.eventSource:"eventThankYou"
      topSelling=guLiveEvents.getEventsByIds(List(
        "16253236869",
        "16253494640",
        "16234171845",
        "16351430569",
        "15351696337",
        "16252865759",
        "16253323127",
        "15926171608",
        "15597340064",
        "16380673034"
      )),
      thisWeek=guLiveEvents.getEventsBetween(new Interval(now, now + 1.week)),
      nextWeek=guLiveEvents.getEventsBetween(new Interval(now + 1.week, now + 2.weeks)),
      recentlyCreated=guLiveEvents.getRecentlyCreated(now - 1.weeks),
      partnersOnly=guLiveEvents.getEvents.filter(_.internalTicketing.exists(_.isCurrentlyAvailableToPaidMembersOnly)),
      programmingPartnerEvents=guLiveEvents.getPartnerEvents
    )

    val latestArticles = GuardianContentService.membershipFrontContent.map(ContentItem)

    Ok(views.html.whatson.overview(pageInfo, events, latestArticles))
  }

  def toSlug(s: String) = {
    s.toLowerCase.replaceAll("[^A-Za-z0-9 ]", "").replace(" ", "-")
  }

  def groupEventsByField(events: Seq[RichEvent], grouper: RichEvent => Option[String]) : Seq[EventFilterLink] = {
    events.groupBy(grouper).collect {
      case (Some(groupName), events) => EventFilterLink(groupName, toSlug(groupName), events.size)
    }.toSeq
  }

  def calendarGrid(paramLocation: Option[String] = None, paramPeriod: Option[String] = None, paramProvider: Option[String] = None) = GoogleAuthenticatedStaffAction { implicit request =>

    val pageInfo = PageInfo(
      CopyConfig.copyTitleEvents,
      request.path,
      Some(CopyConfig.copyDescriptionEvents)
    )

    var allEvents = collectAllEvents

    paramLocation match {
      case None =>        // do nothing
      case Some("all") => // do nothing
      case Some(loc) => {
        allEvents = allEvents.filter(e => {
          toSlug(e.event.venue.address.flatMap(_.city).getOrElse("")).equals(loc)
        })
      }
    }

    paramProvider match {
      case None =>        // do nothing
      case Some("all") => // do nothing
      case Some(p) => {
        allEvents = allEvents.filter(e => {
          toSlug(e.event.providerOpt.getOrElse("")).equals(p)
        })
      }
    }

    val listPeriods = List(
      PeriodGroup("7-days", "Next 7 days", 1.week),
      PeriodGroup("14-days", "Next 14 days", 2.week),
      PeriodGroup("30-days", "Next 30 days", 1.month)
    )

    val periodOpt = listPeriods.find(_.slug == paramPeriod.getOrElse(""))
    allEvents = periodOpt.map { p =>
      val now = DateTime.now
      val interval = new Interval(now, now + p.period)
      allEvents.filter(event => interval.contains(event.start))
    }.getOrElse(allEvents)

    val listLocations = groupEventsByField(allEvents, _.venue.address.flatMap(_.city))
    val listProviders = groupEventsByField(allEvents, _.providerOpt)
    val eventsGroupedByMonth = groupEventsByMonth(allEvents)

    Ok(views.html.whatson.calendarGrid(
      eventsGroupedByMonth,
      pageInfo,
      listLocations,
      paramLocation,
      listPeriods,
      paramPeriod,
      listProviders,
      paramProvider
    ))
  }

  def calendarList = GoogleAuthenticatedStaffAction { implicit request =>

    val pageInfo = PageInfo(
      CopyConfig.copyTitleEvents,
      request.path,
      Some(CopyConfig.copyDescriptionEvents)
    )

    val eventsGroupedByDayAndMonth = groupEventsByDayAndMonth(collectAllEvents)
    Ok(views.html.whatson.calendarList(eventsGroupedByDayAndMonth, pageInfo))
  }

}

object WhatsOn extends WhatsOn {
  val guLiveEvents = GuardianLiveEventService
  val localEvents = LocalEventService
  val masterclassEvents = MasterclassEventService
}
