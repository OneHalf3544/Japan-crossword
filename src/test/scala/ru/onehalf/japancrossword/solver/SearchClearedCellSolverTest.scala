package ru.onehalf.japancrossword.solver

import org.scalatest.FunSuite
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel, Orientation}
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.line.{LineImpl, LineOfModelImpl}

/**
 * <p/>
 * <p/>
 * Created: 20.05.13 21:38
 * <p/>
 * @author OneHalf
 */
class SearchClearedCellSolverTest  extends FunSuite {

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK))
  val FILLED = new FilledCell(Color.BLACK)
  val CLEARED = Cleared

  test("find borders") {

    val metadata = parseLine(Orientation.VERTICAL, "1 4 1")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "1, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    3 to 6 foreach (line(_) = FILLED)

    val result = SearchClearedCellSolver.fillLine(line).toList

    assert(result === LineImpl.parse(".._XXXX_.."))
  }

  test("search cleared cells at border") {

    val metadata = parseLine(Orientation.VERTICAL, "2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    line(4) = FILLED

    val result = SearchClearedCellSolver.fillLine(line).toList

    assert(result === LineImpl.parse("___.X.____"))
  }

  test("fill already solved") {

    val metadata = parseLine(Orientation.VERTICAL, "1 2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 0, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    line(1) = FILLED
    line(4) = FILLED
    line(5) = FILLED

    val result = SearchClearedCellSolver.fillLine(line).toList

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

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    line(2) = CLEARED
    line(5) = CLEARED

    val result = SearchClearedCellSolver.fillLine(line).toList

    assert(result === LineImpl.parse("______...."))
  }

}
