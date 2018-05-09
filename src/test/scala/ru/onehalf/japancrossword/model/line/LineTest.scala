package ru.onehalf.japancrossword.model.line

import org.scalatest.{FlatSpec, Matchers}
import ru.onehalf.japancrossword.model.Cell
import ru.onehalf.japancrossword.model.Cell.{CLEARED, FILLED, NOT_KNOWN}
import ru.onehalf.japancrossword.model.line.LineImpl.parseLine

class LineTest extends FlatSpec with Matchers {

  behavior of "'isSolved' method"

  it should "return true when line doesn't have unknown cells" in {
    val line = parseLine(1, 2, 3, "_X_XX_XXX")

    line.isSolved shouldBe true
  }

  it should "return false when not all cells are resolved"


  behavior of "'notKnownCount' method"

  it should "return zero when line is solved" in {
    val line = parseLine(1, 2, 3, "_X_XX_XXX")

    line.notKnownCount shouldBe 0
  }

  it should "return correct count of unsolved cells" in {
    val line = parseLine(1, 2, 3, "_X.X._X.X")

    line.notKnownCount shouldBe 3
  }

  behavior of "'canStartsWith' method"

  it should "return true when the argument is the same line" in {
    val line = parseLine(1, 2, 3, "_X.X._X.X")

    line.canStartsWith(line) shouldBe true
  }

  it should "return false when the argument line is longer than the current one" in {
    val line = parseLine(1, 1, "_X.X")

    line.canStartsWith(parseLine(1, 2, 3, "_X.X._X.X")) shouldBe false
  }

  it should "return false if the current object cannot start from the argument content" in {
    val line = parseLine(1, 2, 3, "_X.X._X.X")

    line.canStartsWith(parseLine(1, "X")) shouldBe false
  }

  it should "calculate a result with using NOT_KNOWN as a wildcard" in {
    val line = parseLine(1, 2, 3, "_X.X._X.X")

    line.canStartsWith(parseLine(1, 2, "_X_X.")) shouldBe true
    line.canStartsWith(parseLine(1, 2, "_..X.")) shouldBe true
    line.canStartsWith(parseLine(1, "..")) shouldBe true
  }

  behavior of "'countStat' method"

  it should "return one tuple for row of single type" in {
    val stat = Line.countStat(Array(CLEARED, CLEARED, CLEARED, CLEARED, CLEARED))

    stat shouldEqual List((CLEARED, 5))
  }

  it should "count count all types" in {
    val stat = Line.countStat(Array(CLEARED, FILLED, FILLED, NOT_KNOWN, CLEARED))

    stat shouldEqual List((CLEARED, 1), (FILLED, 2), (NOT_KNOWN, 1), (CLEARED, 1))
  }
}
