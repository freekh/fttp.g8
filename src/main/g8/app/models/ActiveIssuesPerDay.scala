package models

import play.api.libs.json._
import org.joda.time.DateTime

/**
 * ActiveIssues per day is the representation of Github Issues per day
 */
object ActiveIssuesPerDay {
  /**
   * This provided method takes a sequence of issues and returns
   * a sequence of active (non closed) issues per day
   *
   * Note: Probably not the best performing implementation, since
   * this one is optimized for readability
   *
   * Example:
   * ActiveIssuesPerDay.calculate(Seq(Issue(.., createdAt = 2012-01-27T..., closedAt = Some(2012-01-29T), ...), Issue(..., createAt = 2012-01-26T, closedAt = None)))
   *
   * yields:
   * Seq((2012-01-26T00:00:00.000+01:00,1), (2012-01-27T00:00:00.000+01:00,2), (2012-01-29T00:00:00.000+01:00,1))
   */
  def calculate(issueArgs: Seq[models.github.Issue]): Seq[(DateTime, Int)] = {
    def roundToDay(d: DateTime) = d.withSecondOfMinute(0).withMinuteOfHour(0).withMillisOfSecond(0).withHourOfDay(0)
    def roundAll(issues: Seq[models.github.Issue]) = issues.map(issue => issue.copy(closedAt = issue.closedAt.map(roundToDay), createdAt = roundToDay(issue.createdAt)))

    val issues = roundAll(issueArgs)

    val amountClosedAt = for { //group the amount of closed issues by dates
      (perhapsClosedAt, issuesPerClosed) <- issues.groupBy(_.closedAt)
      closedAt <- perhapsClosedAt //only take the ones that have closed
    } yield closedAt -> issuesPerClosed.size

    val amountCreatedAt = for { //group the amount of created issues by dates
      (createdAt, issuesPerCreated) <- issues.groupBy(_.createdAt)
    } yield createdAt -> issuesPerCreated.size

    val createdDates = for {
      (date, _) <- amountCreatedAt.toSet
    } yield date
    val closedDates = for {
      (date, _) <- amountCreatedAt.toSet
    } yield date
    val uniqueDates: Set[DateTime] = createdDates ++ closedDates //take the unique dates

    val sortedDates = uniqueDates.toSeq.sortBy(_.toDate)

    var sum = 0
    val activePerDay = for { //sum up the actual active issues per day
      date <- sortedDates
      created = amountCreatedAt.getOrElse(date, 0)
      closed = amountClosedAt.getOrElse(date, 0)
    } yield {
      sum += created - closed
      date -> sum
    }
    activePerDay
  }
}
