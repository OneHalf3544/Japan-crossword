package ru.onehalf.japancrossword.solver

import org.scalatest.FunSuite
import queue.SolveLineQueue
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model.{Cell, LineImpl, JapanCrosswordModel}

/**
 * <p/>
 * <p/>
 * Created: 21.05.13 8:49
 * <p/>
 * @author OneHalf
 */
class SolverTest extends FunSuite {

  test("divideToSublist") {

    val metadata = parseLine(Orientation.VERTICAL, "1 4 1")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "1, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(0) = Cell.FILLED
    line(2) = Cell.CLEARED
    line(5) = Cell.FILLED
    line(7) = Cell.CLEARED
    line(9) = Cell.FILLED

    val splitter = new SolveLineQueue(model).splitter

    val result = splitter.divideToSublists(line, splitter.countStat(line))

    assert(result === List(
      new LineImpl(0, Orientation.HORIZONTAL, model, 0, 2),
      new LineImpl(0, Orientation.HORIZONTAL, model, 2, 5),
      new LineImpl(0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  test("divideToSublist2") {

    val metadata = parseLine(Orientation.VERTICAL, "4 1")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(0) = Cell.CLEARED
    line(2) = Cell.CLEARED
    line(5) = Cell.FILLED
    line(7) = Cell.CLEARED
    line(9) = Cell.FILLED

    val solver = new SolveLineQueue(model).splitter

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new LineImpl(0, Orientation.HORIZONTAL, model, 0, 7),
      new LineImpl(0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  test("divideToSublist3") {

    val metadata = parseLine(Orientation.VERTICAL, "2 2")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(1) = Cell.FILLED
    line(3) = Cell.CLEARED
    line(5) = Cell.FILLED
    line(7) = Cell.CLEARED
    line(8) = Cell.CLEARED
    line(9) = Cell.CLEARED

    val solver = new SolveLineQueue(model).splitter

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new LineImpl(0, Orientation.HORIZONTAL, model, 0, 3),
      new LineImpl(0, Orientation.HORIZONTAL, model, 3, 7)))

  }

}
