package tesl.parser

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import tesl.TE
import tesl.TE.BetweenDatesTE
import tesl.parser.Parser.ParseError

import java.time.LocalDateTime

class ParserTest  extends AnyFlatSpec with Matchers with Inside {

  "BetweenDates" must "parse" in {
    val expr = "BetweenDates(2024-01-01T12:00:00,2024-01-01T12:00:00)"
    inside(TE.decode(expr)) {
      case Right(te) => te mustBe BetweenDatesTE(LocalDateTime.parse("2024-01-01T12:00:00"), LocalDateTime.parse("2024-01-01T12:00:00"))
    }
  }
}
