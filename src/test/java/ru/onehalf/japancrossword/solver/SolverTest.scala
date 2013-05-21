package ru.onehalf.japancrossword.solver

import org.scalatest.FunSuite
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model.{LineTrait, Cell, Line, JapanCrosswordModel}

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

    val line = new Line(0, Orientation.HORIZONTAL, model)
    line(0) = Cell.FILLED
    line(2) = Cell.CLEARED
    line(5) = Cell.FILLED
    line(7) = Cell.CLEARED
    line(9) = Cell.FILLED

    val solver = new Solver(model) {
      def solve() {}
      def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = Nil
    }

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new Line(0, Orientation.HORIZONTAL, model, 0, 2),
      new Line(0, Orientation.HORIZONTAL, model, 2, 5),
      new Line(0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  test("divideToSublist2") {

    val metadata = parseLine(Orientation.VERTICAL, "4 1")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata)

    val line = new Line(0, Orientation.HORIZONTAL, model)
    line(0) = Cell.CLEARED
    line(2) = Cell.CLEARED
    line(5) = Cell.FILLED
    line(7) = Cell.CLEARED
    line(9) = Cell.FILLED

    val solver = new Solver(model) {
      def solve() {}
      def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = Nil
    }

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new Line(0, Orientation.HORIZONTAL, model, 0, 7),
      new Line(0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  test("divideToSublist3") {

    val metadata = parseLine(Orientation.VERTICAL, "2 2")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new Line(0, Orientation.HORIZONTAL, model)
    line(1) = Cell.FILLED
    line(3) = Cell.CLEARED
    line(5) = Cell.FILLED
    line(7) = Cell.CLEARED
    line(8) = Cell.CLEARED
    line(9) = Cell.CLEARED

    val solver = new Solver(model) {
      def solve() {}
      def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = Nil
    }

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new Line(0, Orientation.HORIZONTAL, model, 0, 3),
      new Line(0, Orientation.HORIZONTAL, model, 3, 7)))

  }

}
