import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalacheck.{Arbitrary, Gen}

import java.time.{DayOfWeek, LocalDateTime, LocalTime, Month}

import tesl.TE

class TemporalExpressionTest extends AnyFlatSpec with Matchers {

  "DayOfWeekTE" must "evaluate to true" in {
    val now = LocalDateTime.of(2024, Month.JUNE, 2, 0, 0, 0)

    DayOfWeek.values().zipWithIndex.foreach{case (day, offset) =>
      val date = now.plusDays(offset + 1)
      TE.DayOfWeekTE(day).includes(date) mustBe true
    }

    DayOfWeek.values().zipWithIndex.foreach{case (day, offset) =>
      val date = now.plusDays(offset)
      TE.DayOfWeekTE(day).includes(date) mustBe false
    }
  }

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

  "Always" must "evaluate to true" in {
    for {
      date <- Arbitrary.arbLocalDateTime.arbitrary
    } yield TE.AlwaysTE.includes(date) mustBe true
  }

  "Never" must "evaluate to false" in {
    for {
      date <- Arbitrary.arbLocalDateTime.arbitrary
    } yield TE.NeverTE.includes(date) mustBe false
  }
}
