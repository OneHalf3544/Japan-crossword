package ru.onehalf.japancrossword.model.line

import org.scalatest.{FlatSpec, FunSuite, Matchers}
import ru.onehalf.japancrossword.model.line.LineImpl.parseLine
import ru.onehalf.japancrossword.model.line.LineMetadata.metadata

class LineImplTest extends FlatSpec with Matchers {

  behavior of "'dropFromBegining' method"

  it should "throw error then the current line doesn't start from argument" in {
    val line = parseLine(1, 2, "__X__")

    an [AssertionError] shouldBe thrownBy {
      line.dropFromBegining(parseLine(1, "X"))
    }
  }

  it should "remove the empty part of the line from the begining of the current one" in {
    val line = parseLine(1, 2, "__X___")

    val result = line.dropFromBegining(LineImpl.empty(2))

    result shouldBe parseLine(1, 2, "X___")
  }

  it should "remove the part of the line and the metadata from the begining of the current one" in {
    val line = parseLine(1, 2, "__X____")

    val result = line.dropFromBegining(LineImpl.parseLine(1, "..X."))

    result shouldBe parseLine(2, "___")
  }

  it should "leave an empty line after removing" in {
    val line = parseLine(1, 2, "._X____")

    val result = line.dropFromBegining(LineImpl.parseLine(1, 2, "X.X."))

    result shouldBe LineImpl.empty(3)
  }

  "'solved' method" should "correctly calculate metadata" in {
    val line = LineImpl.solved(LineImpl.parse("X_X_XX_"):_*)

    line.metadata shouldBe metadata(1, 1, 2)
  }

}
