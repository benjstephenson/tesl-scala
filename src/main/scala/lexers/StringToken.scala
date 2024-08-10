package lexers

import scala.util.parsing.input.Positional

enum StringToken extends Positional:
  case DayName(name: String) extends StringToken
  case TimeString(time: String) extends StringToken
  case DateString(date: String) extends StringToken
  case Literal(str: String) extends StringToken
  case Colon() extends StringToken
  case Comma() extends StringToken
  case OpenParen() extends StringToken
  case CloseParen() extends StringToken
  case DayOfWeekId() extends StringToken
  case DateId() extends StringToken
  case AndId() extends StringToken
  case OrId() extends StringToken
  case BetweenDatesId() extends StringToken
  case BetweenTimesId() extends StringToken

enum Token:
  case DayOfWeekToken(day: String) extends Token
  case DateToken(date: String)extends Token
  case AndToken(self: Token, that: Token)extends Token
  case OrToken(self: Token, that: Token)extends Token
  case BetweenDatesToken(from: String, to: String)extends Token
  case BetweenTimesToken(from: String, to: String)extends Token
