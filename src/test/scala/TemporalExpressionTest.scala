import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import java.time.{LocalDateTime, LocalTime, Month}

class TemporalExpressionTest extends AnyFlatSpec with Matchers {

  "BetweenDatesTE" must "evaluate to true" in {
    val now = LocalDateTime.of(2024, Month.JUNE, 10, 10, 30, 0)
    val from = LocalDateTime.of(2024, Month.JUNE, 8, 10, 30, 0)
    val to = LocalDateTime.of(2024, Month.JUNE, 12, 11, 30, 0)

    TE.BetweenDatesTE(from, to).includes(now) mustBe true
  }

  it must "evaluate to false" in {
    val now = LocalDateTime.of(2024, Month.JUNE, 5, 10, 30, 0)
    val from = LocalDateTime.of(2024, Month.JUNE, 8, 10, 30, 0)
    val to = LocalDateTime.of(2024, Month.JUNE, 12, 11, 30, 0)

    TE.BetweenDatesTE(from, to).includes(now) mustBe false
  }

  "BetweenTimesTE" must "evaluate to true" in {
    val now = LocalDateTime.of(2024, Month.JUNE, 10, 10, 30, 0)

    TE.BetweenTimesTE(LocalTime.of(8, 15), LocalTime.of(12, 0))
      .includes(now) mustBe true
  }

  it must "evaluate to false" in {
    val now = LocalDateTime.of(2024, Month.JUNE, 10, 6, 30, 0)

    TE.BetweenTimesTE(LocalTime.of(8, 15), LocalTime.of(12, 0))
      .includes(now) mustBe false
  }
}
