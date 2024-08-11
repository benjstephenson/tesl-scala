package tesl

import lexers.StringLexer
import lexers.StringLexer.LexerError
import parser.Parser
import parser.Parser.ParseError

import java.time.{DayOfWeek, LocalDate, LocalDateTime, LocalTime}

enum TE:
  case DayOfWeekTE(day: DayOfWeek) extends TE
  case DateTE(date: LocalDate) extends TE
  case AndTE(self: TE, that: TE) extends TE
  case OrTE(self: TE, that: TE) extends TE
  case BetweenDatesTE(from: LocalDateTime, to: LocalDateTime) extends TE
  case BetweenTimesTE(from: LocalTime, to: LocalTime) extends TE
  case NotTE(expr: TE) extends TE
  case AlwaysTE extends TE
  case NeverTE extends TE

  def and(that: TE): TE = TE.AndTE(this, that)
  def or(that: TE): TE = TE.OrTE(this, that)

  def includes(dateTime: LocalDateTime): Boolean = this match
    case TE.AlwaysTE => true
    case TE.NeverTE => false
    case TE.NotTE(expr) => !expr.includes(dateTime)
    case TE.DayOfWeekTE(day) => dateTime.getDayOfWeek.equals(day)
    case TE.DateTE(date) => LocalDateTime.from(date).isEqual(dateTime)
    case TE.AndTE(self, that) => self.includes(dateTime) && that.includes(dateTime)
    case TE.OrTE(self, that) => self.includes(dateTime) || that.includes(dateTime)
    case TE.BetweenDatesTE(from, to) => !(dateTime.isBefore(from) || dateTime.isAfter(to))
    case TE.BetweenTimesTE(from, to) =>
      val timeOfDay = LocalTime.from(dateTime).toSecondOfDay
      timeOfDay >= from.toSecondOfDay && timeOfDay <= to.toSecondOfDay

  def print: String = this match
    case TE.AlwaysTE => "Always"
    case TE.NeverTE => "Never"
    case TE.NotTE(expr) => s"Not(${expr.print})"
    case TE.DayOfWeekTE(day) => s"DayOfWeek(${day.name})"
    case TE.DateTE(date) => s"Date(${date.toString})"
    case TE.AndTE(self, that) => s"And(${self.print}, ${that.print})"
    case TE.OrTE(self, that) => s"Or(${self.print}, ${that.print})"
    case TE.BetweenDatesTE(from, to) => s"BetweenDates(${from.toString}, ${to.toString})"
    case TE.BetweenTimesTE(from, to) => s"BetweenTimes(${from.toString}, ${to.toString})"

object TE:
  def decode(str: String): Either[LexerError | ParseError, TE] = for {
    token <- StringLexer(str)
    te <- Parser.parse(token)
  } yield te