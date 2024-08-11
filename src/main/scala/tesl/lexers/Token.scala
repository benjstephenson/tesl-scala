package tesl.lexers

import scala.util.parsing.input.Positional

enum Token extends Positional:
  case AlwaysToken() extends Token
  case NeverToken() extends Token
  case NotToken(expr: Token) extends Token
  case DayOfWeekToken(day: String) extends Token
  case DateToken(date: String)extends Token
  case AndToken(self: Token, that: Token)extends Token
  case OrToken(self: Token, that: Token)extends Token
  case BetweenDatesToken(from: String, to: String)extends Token
  case BetweenTimesToken(from: String, to: String)extends Token
