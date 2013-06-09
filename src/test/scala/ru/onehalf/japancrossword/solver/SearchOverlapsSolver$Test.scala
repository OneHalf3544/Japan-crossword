package ru.onehalf.japancrossword.solver

import org.scalatest.FunSuite
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model._
import java.awt.Color
import ru.onehalf.japancrossword.model.NotKnownCell

/**
 * <p/>
 * <p/>
 * Created: 02.06.13 20:38
 * <p/>
 * @author OneHalf
 */
class SearchOverlapsSolver$Test extends FunSuite {

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK))
  val FILLED = new FilledCell(Color.BLACK)
  val CLEARED = Cleared

  test("fitFromLeft") {
    val metadata = parseLine(Orientation.VERTICAL, "2 1 3")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 0, 0, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)

    val result = SearchOverlapsSolver.fitFromLeft(metadata(0), line).get

    assert(result === List(FILLED, FILLED, CLEARED, FILLED, CLEARED, FILLED, FILLED, FILLED, CLEARED, CLEARED))

  }

  test("fitFromRight") {
    val metadata = parseLine(Orientation.VERTICAL, "2 1 3")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 0, 0, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)

    val result = SearchOverlapsSolver.fitFromRight(metadata(0), line).get

    assert(result === List(CLEARED, CLEARED, FILLED, FILLED, CLEARED, FILLED, CLEARED, FILLED, FILLED, FILLED))

  }

  test("fillLine") {
    val metadata = parseLine(Orientation.VERTICAL, "2 1 3")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 0, 0, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)

    val result = SearchOverlapsSolver.fillLine(metadata(0), line)

    assert(result === List(NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, FILLED, NOT_KNOWN, NOT_KNOWN))

  }

  test("fillLine2") {
    val metadata = parseLine(Orientation.VERTICAL, "2 2 13 4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(9) = FILLED
    (11 to 18) foreach(line(_) = FILLED)
    line(20) = FILLED
    line(24) = FILLED

    val result = SearchOverlapsSolver.fillLine(metadata(0), line)

    assert(result === List(
      NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN,
      FILLED, FILLED, FILLED, FILLED, FILLED, FILLED, FILLED, FILLED, FILLED, FILLED, FILLED, FILLED,
      NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, FILLED, FILLED, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN))
  }

}
