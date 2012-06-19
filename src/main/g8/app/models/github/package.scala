package models

package object github {
  lazy val baseUrl = "https://api.github.com"

  //Usage: timeParser.parseDateTime
  lazy val timeParser = org.joda.time.format.ISODateTimeFormat.dateTimeNoMillis
}
