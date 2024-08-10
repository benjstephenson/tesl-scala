import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import java.time.DayOfWeek
import java.time.LocalTime

class TemporalToStringTest extends AnyFlatSpec with Matchers {

  "DayOfWeek" must "render to string" in {
    TE.DayOfWeekTE(DayOfWeek.MONDAY).print mustBe "DayOfWeek(MONDAY)"
  }

  "And" must "render to string" in {
    TE.DayOfWeekTE(DayOfWeek.MONDAY)
      .and(TE.DayOfWeekTE(DayOfWeek.WEDNESDAY))
      .print mustBe "And(DayOfWeek(MONDAY), DayOfWeek(WEDNESDAY))"
  }

  "Or" must "render to string" in {
    TE.DayOfWeekTE(DayOfWeek.MONDAY)
      .or(TE.DayOfWeekTE(DayOfWeek.WEDNESDAY))
      .print mustBe "Or(DayOfWeek(MONDAY), DayOfWeek(WEDNESDAY))"
  }

  "BetweenTimes" must "render to string" in {
    TE.BetweenTimesTE(
      LocalTime.of(5, 30),
      LocalTime.of(14, 55)
    ).print mustBe "BetweenTimes(05:30, 14:55)"
  }
}
