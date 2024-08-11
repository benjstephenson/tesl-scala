package tesl.parser

import tesl.TE
import tesl.lexers.Token

import java.time.{DayOfWeek, LocalDate, LocalDateTime, LocalTime}
import scala.util.Try

object Parser {

  case class ParseError(input: String, error: String)

  def parse(token: Token): Either[ParseError, TE] = token match {
    case Token.AlwaysToken() => Right(TE.AlwaysTE)
    case Token.NeverToken() => Right(TE.NeverTE)
    case Token.NotToken(expr) => parse(expr).map(TE.NotTE.apply)
    case Token.DayOfWeekToken(day) => parseDayOfWeek(day).map(TE.DayOfWeekTE.apply)
    case Token.DateToken(date) => parseDate(date).map(TE.DateTE.apply)
    case Token.AndToken(self, that) => for {
      _self <- parse(self)
      _that <- parse(that)
    } yield TE.AndTE(_self, _that)

    case Token.OrToken(self, that) => for {
      _self <- parse(self)
      _that <- parse(that)
    } yield TE.OrTE(_self, _that)

    case Token.BetweenDatesToken(from, to) => for {
      _from <- parseDateTime(from)
      _to <- parseDateTime(to)
    } yield TE.BetweenDatesTE(_from, _to)

    case Token.BetweenTimesToken(from, to) => for {
      _from <- parseTime(from)
      _to <- parseTime(to)
    } yield TE.BetweenTimesTE(_from, _to)
  }

  def parseDayOfWeek(str: String): Either[ParseError, DayOfWeek] = str.capitalize match {
    case "MON" | "MONDAY" => Right(DayOfWeek.MONDAY)
    case "TUE" | "TUESDAY" => Right(DayOfWeek.TUESDAY)
    case "WED" | "WEDNESDAY" => Right(DayOfWeek.WEDNESDAY)
    case "THU" | "THURSDAY" => Right(DayOfWeek.THURSDAY)
    case "FRI" | "FRIDAY" => Right(DayOfWeek.FRIDAY)
    case "SAT" | "SATURDAY" => Right(DayOfWeek.SATURDAY)
    case "SUN" | "SUNDAY" => Right(DayOfWeek.SUNDAY)
    case str => Left(ParseError(str, s"Invalid day of week"))
  }

  def parseTime(str: String): Either[ParseError, LocalTime] =
    Try(LocalTime.parse(str)).toEither.left.map(err => ParseError(str, err.getMessage))

  def parseDate(str: String): Either[ParseError, LocalDate] =
    Try(LocalDate.parse(str)).toEither.left.map(err => ParseError(str, err.getMessage))

  def parseDateTime(str: String): Either[ParseError, LocalDateTime] =
    Try(LocalDateTime.parse(str)).toEither.left.map(err => ParseError(str, err.getMessage))
}
