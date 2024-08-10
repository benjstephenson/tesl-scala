package lexers

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object StringLexer extends RegexParsers:

  case class LexerError(location: Location, msg: String)
  case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
  }

  import StringToken.*
  import Token.*

  override val whiteSpace: Regex = "[ \t\n\r\f]+".r

  override def skipWhitespace = true

  def apply(code: String): Either[LexerError, Token] = {
    parse(phrase(complexParser), code) match {
      case NoSuccess.I(msg, next) =>
        Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
  
  def dateExprParser = date ~ openParen ~ dateString ~ closeParen ^^ {
    case _ ~ _ ~ date ~ _ => DateToken(date.date)
  }

  def betweenDatesExprParser = betweenDates ~ openParen ~ dateString ~ comma ~ dateString ~ closeParen ^^ {
    case _ ~ a_ ~ startDate ~ _ ~ endDate ~ _ => BetweenDatesToken(startDate.date, endDate.date)
  }

  def betweenTimesExprParser = betweenTimes ~ openParen ~ timeString ~ comma ~ timeString ~ closeParen ^^ {
    case _ ~ a_ ~ startTime ~ _ ~ endTime ~ _ => BetweenTimesToken(startTime.time, endTime.time)
  }

  def dayOfWeekExprParser = dayOfWeek ~ openParen ~ dayName ~ closeParen ^^ {
    case _ ~ _ ~ dayName ~ _ => DayOfWeekToken(dayName.name)
  }

  def andExprParser = and ~ openParen ~ complexParser ~ comma ~ complexParser ~ closeParen ^^ {
    case _ ~ _ ~ expr1 ~ _ ~ expr2 ~ _ => AndToken(expr1, expr2)
  }

  def orExprParser = or ~ openParen ~ complexParser ~ comma ~ complexParser ~ closeParen ^^ {
    case _ ~ _ ~ expr1 ~ _ ~ expr2 ~ _ => OrToken(expr1, expr2)
  }

  def singleExprParser = dateExprParser | betweenDatesExprParser | betweenTimesExprParser | dayOfWeekExprParser

  def complexParser: Parser[Token] = andExprParser | orExprParser | singleExprParser

  def comma: Parser[Comma] = positioned {
    "," ^^ { _ => Comma() }
  }

  def openParen: Parser[OpenParen] = positioned {
    "(" ^^ { _ => OpenParen() }
  }

  def closeParen: Parser[CloseParen] = positioned {
    ")" ^^ { _ => CloseParen() }
  }

  def date: Parser[DateId] = positioned {
    "Date" ^^ { _ => DateId() }
  }

  def and: Parser[AndId] = positioned {
    "And" ^^ { _ => AndId() }
  }

  def or: Parser[OrId] = positioned {
    "Or" ^^ { _ => OrId() }
  }

  def betweenDates: Parser[BetweenDatesId] = positioned {
    "BetweenDates" ^^ { _ => BetweenDatesId() }
  }

  def betweenTimes: Parser[BetweenTimesId] = positioned {
    "BetweenTimes" ^^ { _ => BetweenTimesId() }
  }

  def timeString: Parser[TimeString] = positioned {
    "(0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]".r ^^ { time => TimeString(time) }
  }

  def dateString: Parser[DateString] = positioned {
    "(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]{3})?(Z)?".r
    //"(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])".r
      ^^ { date => DateString(date) }
  }

  def dayOfWeek: Parser[DayOfWeekId] = positioned {
    "DayOfWeek" ^^ { _ => DayOfWeekId() }
  }

  def dayName: Parser[DayName] = positioned {
    "(?i)\\b(?:Tue(?:sday)?|Wed(?:nesday)?|Thu(?:rsday)?|Sat(?:urday)?|(Mon|Fri|Sun)(?:day)?)".r ^^
      { dayName => DayName(dayName) }
  }
