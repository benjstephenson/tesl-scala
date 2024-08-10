package lexers

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import java.time.DayOfWeek

class StringLexerTest extends AnyFlatSpec with Inside with Matchers with TableDrivenPropertyChecks {
  import Token._

  "BetweenDates" must "tokenise" in {
    val DateStrings = Table(
      ("date", "desc", "pass"),
      ("2024-01-01T12:00:00.000Z", "full ISO8601", true),
      ("2024-01-01T12:00:00.000", "without Z", true),
      ("2024-01-01T12:00:00", "without millis", true),
      ("2024-01-01T12:00", "without seconds", false),
    )
    forAll(DateStrings) { (date, desc, shouldPass) =>
      inside(StringLexer.apply(s"BetweenDates($date,$date)")) {
        case Right(tokens) if shouldPass => tokens mustBe BetweenDatesToken(date, date)
        case Left(err) if !shouldPass =>
      }
    }
  }


  "BetweenTimes" must "tokenise" in {
    val TimeStrings = Table(
      ("time", "desc", "pass"),
      ("12:00", "full", true),
      ("29:70", "invalid hour/minute", false),
      ("12:", "without minutes", false),
      ("12", "hour only", false),
    )
    forAll(TimeStrings) { (time, desc, shouldPass) =>
      inside(StringLexer.apply(s"BetweenTimes($time, $time)")) {
        case Right(token) if shouldPass => token mustBe BetweenTimesToken(time, time)
        case Left(err) if !shouldPass =>
      }
    }
  }


  "Day of week string" must "tokenise" in {
    val TimeStrings = Table(
      ("day", "desc", "pass"),
      ("Mon", "Mon", true),
      ("Tue", "Tue", true),
      ("Wed", "Wed", true),
      ("Thu", "Thu", true),
      ("Fri", "Fri", true),
      ("Sat", "Sat", true),
      ("Sun", "Sun", true),
    ) ++ DayOfWeek.values().map(d => (d.name, d.name, true))

    forAll(TimeStrings) { (day, desc, shouldPass) =>
      inside(StringLexer.apply(s"DayOfWeek($day)")) {
        case Right(token) if shouldPass => token mustBe DayOfWeekToken(day)
        case Left(err) if !shouldPass =>
      }
    }
  }


  "BetweenDates expressions" must "tokenise" in {
    val expr = "BetweenDates(2024-01-01T12:00:00,2024-01-01T12:00:00)"

    inside(StringLexer(expr)) {
      case Right(token) => token mustBe BetweenDatesToken("2024-01-01T12:00:00", "2024-01-01T12:00:00")
    }
  }

  "And expressions" must "tokenise" in {
    val expr = "And(BetweenDates(2024-01-01T12:00:00, 2024-01-01T12:00:00), DayOfWeek(Tue))"

    inside(StringLexer(expr)) {
      case Right(token) => token mustBe AndToken(BetweenDatesToken("2024-01-01T12:00:00","2024-01-01T12:00:00"), DayOfWeekToken("Tue"))
    }
  }


  "Or expressions" must "tokenise" in {
    val expr = "Or(BetweenDates(2024-01-01T12:00:00, 2024-01-01T12:00:00), DayOfWeek(Tue))"

    inside(StringLexer(expr)) {
      case Right(token) => token mustBe OrToken(BetweenDatesToken("2024-01-01T12:00:00","2024-01-01T12:00:00"), DayOfWeekToken("Tue"))
    }
  }


  "Nested expressions" must "tokenise" in {
    val expr = """
      And(
           Or(
              BetweenDates(2024-01-01T12:00:00, 2024-01-02T12:00:00),
              BetweenDates(2024-01-02T12:00:00, 2024-01-03T12:00:00)
           ),
           Or(DayOfWeek(Tue), DayOfWeek(Thu))
      )
    """.stripMargin

    inside(StringLexer(expr)) {
      case Right(token) => token mustBe AndToken(
        OrToken(
          BetweenDatesToken("2024-01-01T12:00:00","2024-01-02T12:00:00"),
          BetweenDatesToken("2024-01-02T12:00:00","2024-01-03T12:00:00")
        ),
        OrToken(DayOfWeekToken("Tue"), DayOfWeekToken("Thu"))
      )
    }
  }
}
