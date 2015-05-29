package model

import model.RichEvent.RichEvent
import org.joda.time.Period

// used for arbitrary groupings of events with custom titles
case class EventGroup(sequenceTitle: String, events: Seq[RichEvent])

case class EventPortfolio(
    orderedEvents: Seq[RichEvent],
    normal: Seq[RichEvent],
    pastEvents: Option[Seq[RichEvent]],
    otherEvents: Option[EventGroup]
  ) {
  lazy val heroOpt = orderedEvents.headOption
  lazy val priority = orderedEvents.drop(1)
}

case class EventCollections(
    trending: Seq[RichEvent],
    topSelling: Seq[RichEvent],
    thisWeek: Seq[RichEvent],
    nextWeek: Seq[RichEvent],
    recentlyCreated: Seq[RichEvent],
    partnersOnly: Seq[RichEvent],
    programmingPartnerEvents: Option[EventGroup]
)

case class EventFilterLink(name: String, slug: String, count: Int)

case class PeriodGroup(slug: String, name: String, period: Period)
