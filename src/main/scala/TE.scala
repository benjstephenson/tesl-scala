import TE.{AndTE, OrTE}

import java.time.{DayOfWeek, LocalDateTime, LocalTime}

//case class Time(hour: Int, minute: Int) {
//  override def toString: String = s"${Time.zeroPad(hour)}:${Time.zeroPad(minute)}"
//}
//object Time:
//  private def zeroPad(n: Int): String = if n <= 9 then s"0$n" else n.toString
//
enum TE:
  case DayOfWeekTE(day: DayOfWeek) extends TE
  case DateTE(date: LocalDateTime) extends TE
  case AndTE(self: TE, that: TE) extends TE
  case OrTE(self: TE, that: TE) extends TE
  case BetweenDatesTE(from: LocalDateTime, to: LocalDateTime) extends TE
  case BetweenTimesTE(from: LocalTime, to: LocalTime) extends TE

extension (self: TE)
  def and(that: TE): TE = AndTE(self, that)
  def or(that: TE): TE = OrTE(self, that)

  def includes(dateTime: LocalDateTime): Boolean = self match
    case TE.DayOfWeekTE(day) => dateTime.getDayOfWeek.equals(day)
    case TE.DateTE(date) => dateTime.isEqual(date)
    case TE.AndTE(self, that) => self.includes(dateTime) && that.includes(dateTime)
    case TE.OrTE(self, that) => self.includes(dateTime) || that.includes(dateTime)
    case TE.BetweenDatesTE(from, to) => !(dateTime.isBefore(from) || dateTime.isAfter(to))
    case TE.BetweenTimesTE(from, to) =>
      val timeOfDay = LocalTime.from(dateTime).toSecondOfDay
      timeOfDay >= from.toSecondOfDay && timeOfDay <= to.toSecondOfDay

  def print: String = self match
    case TE.DayOfWeekTE(day) => day.name
    case TE.DateTE(date) => date.toString
    case TE.AndTE(self, that) => s"And(${self.print}, ${that.print})"
    case TE.OrTE(self, that) => s"Or(${self.print}, ${that.print})"
    case TE.BetweenDatesTE(from, to) => s"BetweenDates(${from.toString}, ${to.toString})"
    case TE.BetweenTimesTE(from, to) => s"BetweenTimes(${from.toString}, ${to.toString})"
