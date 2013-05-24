package ru.onehalf.japancrossword.solver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model.{Cell, Line, JapanCrosswordModel}

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 2:10
 * <p/>
 * @author OneHalf
 */
class BorderSolverTest extends FlatSpec with ShouldMatchers {

  it should "set CLEARED cells status to model" in {

    val metadata = parseLine(Orientation.VERTICAL, "2 4")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new Line(0, Orientation.HORIZONTAL, model)

    line(1) = Cell.FILLED // Закрашиваем две клетки:          _X_______X
    line(9) = Cell.FILLED // После подбора строки должно быть _X_...XXXX

    val result = VariantsEnumerationSolver.fitRemainder(metadata(0), line).get

    assert(result === List(
      Cell.NOT_KNOWN, Cell.FILLED, Cell.NOT_KNOWN, Cell.CLEARED, Cell.CLEARED,
      Cell.CLEARED, Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED))
  }

}
