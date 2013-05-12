package ru.onehalf.japancrossword.solver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ru.onehalf.japancrossword.model.{Cell, Line, JapanCrosswordModel}
import ru.onehalf.japancrossword.Main.parseLine

/**
 * <p/>
 * <p/>
 * Created: 12.05.13 15:31
 * <p/>
 * @author OneHalf
 */
class SolverTest extends FlatSpec with ShouldMatchers {

  it should "resolve center cells in line" in {

    val metadata = parseLine(Orientation.VERTICAL, "8")
    val model = new JapanCrosswordModel(
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 1, 1, 0, 0"),  // 10 cells
      metadata)

    val line = new Line(0, Orientation.HORIZONTAL, model)

    val result = new Solver(model).fitRemainder(metadata(0), line).get

    assert(result.size === 10)
    assert(result(0) === Cell.NOT_KNOWN)
    assert(result(1) === Cell.NOT_KNOWN)

    assert(2 to 7 forall(result(_) == Cell.FILLED))

    assert(result(8) === Cell.NOT_KNOWN)
    assert(result(9) === Cell.NOT_KNOWN)
  }

  it should "resolve all cells in line" in {

    val metadata = parseLine(Orientation.VERTICAL, "8")
    val model = new JapanCrosswordModel(
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 1, 1, 0, 0"),  // 10 cells
      metadata)

    new Solver(model).solve()

    assert(model.isSolved)
  }

  it should "set CLEARED cells status to model" in {

    val metadata = parseLine(Orientation.VERTICAL, "2 4")
    val model = new JapanCrosswordModel(
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata)

    model(1, 0) = Cell.FILLED // Закрашиваем две клетки:          _X_______X
    model(9, 0) = Cell.FILLED // После подбора строки должно быть _X_...XXXX

    val line = new Line(0, Orientation.HORIZONTAL, model)

    val result = new Solver(model).fitRemainder(metadata(0), line).get

    assert(result === List(
      Cell.NOT_KNOWN, Cell.FILLED, Cell.NOT_KNOWN, Cell.CLEARED, Cell.CLEARED,
      Cell.CLEARED, Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED))
  }

  it should "fill all line by Cell.FILLED status" in {

    val metadata = parseLine(Orientation.VERTICAL, "5")
    val model = new JapanCrosswordModel(
      parseLine(Orientation.HORIZONTAL, "1, 1, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new Line(0, Orientation.HORIZONTAL, model)

    val result = new Solver(model).fitRemainder(metadata(0), line).get

    assert(result === List(Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED))
  }
}
