package ru.onehalf.japancrossword.solver

import org.scalatest.{FlatSpec, Matchers}
import ru.onehalf.japancrossword.model.Cell.{FILLED, NOT_KNOWN}
import ru.onehalf.japancrossword.model.line.LineImpl
import ru.onehalf.japancrossword.model.line.LineImpl.parseLine
import ru.onehalf.japancrossword.model.line.LineMetadata.metadata
import ru.onehalf.japancrossword.solver.SearchOverlapsSolver.{fitFromLeft, fitFromRight}

/**
 * Created: 02.06.13 20:38
 * <p/>
 * @author OneHalf
 */
class SearchOverlapsSolver$Test extends FlatSpec with Matchers {

  "method 'fitFromLeft'" should "fill the line from the start" in {
    val line = parseLine(2, 1, 3, "..........")

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK))
  val FILLED = new FilledCell(Color.BLACK)
  val CLEARED = Cleared

    val result = fitFromLeft(line).get.toList

    result shouldEqual LineImpl.parse("XX_X_XXX__")
  }

  it should "return the line started from an empty cell when it cannot be started from filled one" in {
    val line = parseLine(2, 1, 3, ".XX.......")

    val result = fitFromLeft(line).get.toList

    result shouldEqual LineImpl.parse("_XX_X_XXX_")
  }

  it should "fill the line in the same way as 'fitFromRight' method when no one is allowed" in {
    val line = parseLine(2, 1, 3, ".XX.......")

    val result = fitFromLeft(line).get.toList

    result shouldEqual LineImpl.parse("_XX_X_XXX_")
  }

  it should "return empty line when metadata is empty" in {
    val line = parseLine(metadata(), ".....")

    val result = fitFromLeft(line).get

    result shouldEqual LineImpl.empty(5)
  }

  "method 'fitFromRight'" should "fill the line from the end" in {
    val line = parseLine(2, 1, 3, "..........")

    val result = fitFromRight(line).get.toList

    result shouldEqual LineImpl.parse("__XX_X_XXX")

  }

  "solver" should "find the filled cell" in {
    val line = parseLine(2, 1, 3, "..........")

    val result = SearchOverlapsSolver.fillLine(line).toList

    result shouldEqual LineImpl.parse(".......X..")
  }

  it should "fill the part of line (only one number is in the metadata, bounds are known)" in {
    val line = parseLine(3, "...X.X...")

    val result = SearchOverlapsSolver.fillLine(line)

    result shouldEqual parseLine(line.metadata, "...XXX...")
  }

  it should "fill the part of line (two numbers are in the metadata)" in {
    val line = parseLine(13, 4, "...X.XXXXXXXX.X..X.....")

    val result = SearchOverlapsSolver.fillLine(line)

    //                                                          "...X.XXXXXXXX.X..X....."
    fitFromLeft(line).get  shouldEqual parseLine(line.metadata, "XXXXXXXXXXXXX_XXXX_____")
    fitFromRight(line).get shouldEqual parseLine(line.metadata, "___XXXXXXXXXXXXX_XXXX__")
    result                 shouldEqual parseLine(line.metadata, "...XXXXXXXXXX....X.....")
  }

  it should "fill the part of line " in {
    val line = parseLine(2, 2, 13, 4, ".........X.XXXXXXXX.X..X....")

    val result = SearchOverlapsSolver.fillLine(line)

    //                                                          ".........X.XXXXXXXX.X..X...."
    fitFromLeft(line).get  shouldEqual parseLine(line.metadata, "XX_XX_XXXXXXXXXXXXX_XXXX____")
    fitFromRight(line).get shouldEqual parseLine(line.metadata, "___XX_XX_XXXXXXXXXXXXX_XXXX_")
    result                 shouldEqual parseLine(line.metadata, ".........XXXXXXXXXX....X....")
  }

  it should "throw exception if it is impossible to fill the line" in {
    val line = parseLine(2, ".X.X.")

    a [AssertionError] should be thrownBy {
      SearchOverlapsSolver.fillLine(line)
    }
  }

  it should "fill the whole line when it needed" in {
    val line = parseLine(10, "..........")

    val result = SearchOverlapsSolver.fillLine(line).toList

    result shouldEqual LineImpl.parse("XXXXXXXXXX")
  }

  it should "fill the whole line when it is not ambiguous" in {
    val line = parseLine(4, 5, "..........")

    val result = SearchOverlapsSolver.fillLine(line).toList

    // Don't fill 5th with cleared cell (for sake of simplification this solver). LineSplitter can finish this line.
    result shouldEqual LineImpl.parse("XXXX.XXXXX")
  }

  "numerateChunks" should "work correctly when starts from a FILLED cell" in {
    val line = parseLine(2, 3, "XX_XXX____")

    val result: Seq[Int] = SearchOverlapsSolver.numerateChunksInLine(line, identity)

    result shouldEqual Array(1, 1, 0, 2, 2, 2, 0, 0, 0, 0).toSeq
  }

  it should "work correctly when starts from a CLEARED cell" in {
    val line = parseLine(2, 3, "_XX_XXX___")

    val result: Seq[Int] = SearchOverlapsSolver.numerateChunksInLine(line, identity)

    result shouldEqual Array(0, 1, 1, 0, 2, 2, 2, 0, 0, 0).toSeq
  }
}
