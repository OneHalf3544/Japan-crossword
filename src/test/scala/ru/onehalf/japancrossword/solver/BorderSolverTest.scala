package ru.onehalf.japancrossword.solver

import org.scalatest.{FlatSpec, Matchers}
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model.line.{LineImpl, LineOfModelImpl}
import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel, Orientation}

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 2:10
 * <p/>
 * @author OneHalf
 */
class BorderSolverTest extends FlatSpec with Matchers {

  it should "set CLEARED cells status to model" in {

    val metadata = parseLine(Orientation.VERTICAL, "2 4")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    line(1) = Cell.FILLED // Закрашиваем две клетки:          _X_______X
    line(9) = Cell.FILLED // После подбора строки должно быть _X_...XXXX

    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === LineImpl.parse(".X.___XXXX"))
  }

}
