package views

import com.github.nscala_time.time.Imports._
import org.joda.time.Instant

object Dates {

  implicit class RichInstant(dateTime: Instant) {
    val formatter = DateTimeFormat.mediumDateTime()

    lazy val pretty = formatter.print(dateTime)
  }

  implicit class RichDateTime(dateTime: DateTime) {
    val eventDateTimeFormat = DateTimeFormat.forPattern("MMMM d, y k:mm a")
    lazy val pretty = eventDateTimeFormat.print(dateTime).replace("AM", "am").replace("PM", "pm")
  }

  def todayDay = addSuffix(DateTime.now.toString("dd").toInt)

  def addSuffix(day: Int): String = {
    val suffix = (day % 10) match {
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case _ => "th"
    }
    day + suffix
  }
}
