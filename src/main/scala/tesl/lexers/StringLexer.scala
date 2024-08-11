package tesl.lexers

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object StringLexer extends RegexParsers:

  override val whiteSpace: Regex = "[ \t\n\r\f]+".r
  val timeString: Regex = "(0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]".r
  val dateString: Regex =
    "(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]{3})?(Z)?".r
  val dayName: Regex =
    "(?i)\\b(?:Tue(?:sday)?|Wed(?:nesday)?|Thu(?:rsday)?|Sat(?:urday)?|(Mon|Fri|Sun)(?:day)?)".r

  override def skipWhitespace = true

  import Token.*

  def apply(code: String): Either[LexerError, Token] = {
    parse(phrase(complexParser), code) match {
      case NoSuccess.I(msg, next) =>
        Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def singleExprParser: Parser[Token] = dateExprParser | betweenDatesExprParser | betweenTimesExprParser | dayOfWeekExprParser

  def complexParser: Parser[Token] = notExprParser | andExprParser | orExprParser | singleExprParser

  def notExprParser: Parser[Token] = positioned {
    "Not" ~>! "(" ~>! complexParser <~! ")" ^^ { expr => NotToken(expr) }
  }

  def dayOfWeekExprParser: Parser[Token] = positioned {
    "DayOfWeek" ~>! "(" ~>! dayName <~! ")" ^^ { dayName => DayOfWeekToken(dayName) }
  }

  def dateExprParser: Parser[Token] = positioned {
    "Date" ~>! "(" ~>! dateString <~! ")" ^^ {
      date => DateToken(date)
    }
  }

  def betweenDatesExprParser: Parser[Token] = positioned {
    "BetweenDates" ~>! "(" ~>! dateString ~ "," ~ dateString <~! ")" ^^ {
      case from ~ _ ~ to => BetweenDatesToken(from, to)
    }
  }

  def betweenTimesExprParser: Parser[Token] = positioned {
    "BetweenTimes" ~>! "(" ~>! timeString ~ "," ~ timeString <~! ")" ^^ {
      case startTime ~ _ ~ endTime => BetweenTimesToken(startTime, endTime)
    }
  }

  def andExprParser: Parser[Token] = positioned {
    "And" ~>! "(" ~>! complexParser ~ "," ~ complexParser <~! ")" ^^ {
      case expr1 ~ _ ~ expr2 => AndToken(expr1, expr2)
    }
  }

  def orExprParser: Parser[Token] = positioned {
    "Or" ~>! "(" ~>! complexParser ~ "," ~ complexParser <~! ")" ^^ {
      case expr1 ~ _ ~ expr2 => OrToken(expr1, expr2)
    }
  }

  case class LexerError(location: Location, msg: String)

  case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
  }
