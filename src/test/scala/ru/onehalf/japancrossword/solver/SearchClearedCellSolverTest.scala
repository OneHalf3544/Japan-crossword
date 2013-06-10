package ru.onehalf.japancrossword.solver

import org.scalatest.FunSuite
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model._
import java.awt.Color
import ru.onehalf.japancrossword.model.NotKnownCell

/**
 * <p/>
 * <p/>
 * Created: 20.05.13 21:38
 * <p/>
 * @author OneHalf
 */
class SearchClearedCellSolverTest  extends FunSuite {

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK), true)
  val FILLED = new FilledCell(Color.BLACK)
  val CLEARED = Cleared

  test("find borders") {

    val metadata = parseLine(Orientation.VERTICAL, "1 4 1")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "1, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    3 to 6 foreach (line(_) = FILLED)

    val result = SearchClearedCellSolver.fillLine(metadata(0), line)

    assert(result === List(
      NOT_KNOWN, NOT_KNOWN,
      CLEARED,
      FILLED, FILLED, FILLED, FILLED,
      CLEARED,
      NOT_KNOWN, NOT_KNOWN))
  }

  test("search cleared cells at border") {

    val metadata = parseLine(Orientation.VERTICAL, "2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(4) = FILLED

    val result = SearchClearedCellSolver.fillLine(metadata(0), line)

    assert(result === List(
      CLEARED, CLEARED, CLEARED,
      NOT_KNOWN, FILLED, NOT_KNOWN,
      CLEARED, CLEARED, CLEARED, CLEARED))
  }

  test("fill already solved") {

    val metadata = parseLine(Orientation.VERTICAL, "1 2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 0, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(1) = FILLED
    line(4) = FILLED
    line(5) = FILLED

    val result = SearchClearedCellSolver.fillLine(metadata(0), line)

    assert(result === List(
      CLEARED,
      FILLED,
      CLEARED, CLEARED,
      FILLED, FILLED,
      CLEARED, CLEARED, CLEARED, CLEARED))
  }

  test("fill cells betwean cleared ") {

    val metadata = parseLine(Orientation.VERTICAL, "4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(2) = CLEARED
    line(5) = CLEARED

    val result = SearchClearedCellSolver.fillLine(metadata(0), line)

    assert(result === List(
      CLEARED, CLEARED, CLEARED, CLEARED, CLEARED, CLEARED,
      NOT_KNOWN, NOT_KNOWN, NOT_KNOWN, NOT_KNOWN))
  }

}
