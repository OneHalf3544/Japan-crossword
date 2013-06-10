package ru.onehalf.japancrossword.solver

import org.scalatest.FunSuite
import queue.{SolveQueueTask, NonogramSolverQueue}
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model._
import java.awt.Color

/**
 * <p/>
 * <p/>
 * Created: 21.05.13 8:49
 * <p/>
 * @author OneHalf
 */
class LineSplitterTest extends FunSuite {

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK), true)
  val FILLED = new FilledCell(Color.BLACK)
  val CLEARED = Cleared

  test("divideToSublist") {

    val metadata = parseLine(Orientation.VERTICAL, "1 4 1")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "1, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(0) = FILLED
    line(2) = CLEARED
    line(5) = FILLED
    line(7) = CLEARED
    line(9) = FILLED

    val splitter = new NonogramSolverQueue(model, "", new ModelSolver(model)).splitter

    val result = splitter.divideToSublists(line, splitter.countStat(line))

    assert(result === List(
      new LineImpl(0, Orientation.HORIZONTAL, model, 0, 2),
      new LineImpl(0, Orientation.HORIZONTAL, model, 2, 5),
      new LineImpl(0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  test("divideToSublist2") {

    val metadata = parseLine(Orientation.VERTICAL, "4 1")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 0, 1, 1, 1, 1, 0, 0, 1"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(0) = CLEARED
    line(2) = CLEARED
    line(5) = FILLED
    line(7) = CLEARED
    line(9) = FILLED

    val solver = new NonogramSolverQueue(model, "", new ModelSolver(model)).splitter

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new LineImpl(0, Orientation.HORIZONTAL, model, 0, 7),
      new LineImpl(0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  test("divideToSublist3") {

    val metadata = parseLine(Orientation.VERTICAL, "2 2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(1) = FILLED
    line(3) = CLEARED
    line(5) = FILLED
    line(7) = CLEARED
    line(8) = CLEARED
    line(9) = CLEARED

    val solver = new NonogramSolverQueue(model, "", new ModelSolver(model)).splitter

    val result = solver.divideToSublists(line, solver.countStat(line))

    assert(result === List(
      new LineImpl(0, Orientation.HORIZONTAL, model, 0, 3),
      new LineImpl(0, Orientation.HORIZONTAL, model, 3, 7)))

  }

  test("drop cleared cells from ends") {
    val metadata = parseLine(Orientation.VERTICAL, "2 2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(0) = CLEARED
    line(8) = CLEARED
    line(9) = CLEARED

    val splitter = new NonogramSolverQueue(model, "", new ModelSolver(model)).splitter

    val result = splitter.dropClearedFromEnds(line)

    assert(result === new LineImpl(0, Orientation.HORIZONTAL, model, 1, 7))
  }

  test("split line by max part") {
    val metadata = parseLine(Orientation.VERTICAL, "1 2 1")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 0, 0, 1, 1, 0, 1, 0, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(3) = CLEARED
    line(4) = FILLED
    line(5) = FILLED
    line(6) = CLEARED

    val solver = new NonogramSolverQueue(model, "", new ModelSolver(model))
    solver.splitter.splitByFirstMaxLength(line, metadata(0), BorderSolver)

    assert(solver.queue.size() === 2)
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(1, Color.BLACK), new LineImpl(0, Orientation.HORIZONTAL, model, 0, 4), BorderSolver))
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(1, Color.BLACK), new LineImpl(0, Orientation.HORIZONTAL, model, 6, 4), BorderSolver))
  }

  test("drop chanks from ends") {
    val metadata = parseLine(Orientation.VERTICAL, "3 2 1")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "1, 1, 1, 0, 1, 1, 0, 1, 0, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(0) = FILLED
    line(1) = FILLED
    line(2) = FILLED
    line(3) = CLEARED

    val solver = new NonogramSolverQueue(model, "", new ModelSolver(model))
    solver.splitter.dropChanksFromEnds(metadata(0), line, BorderSolver)

    assert(solver.queue.size() === 1)
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(Array(2, 1)), new LineImpl(0, Orientation.HORIZONTAL, model, 4, 6), BorderSolver))
  }

  test("split by known chank") {
    val metadata = parseLine(Orientation.VERTICAL, "2 1 2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 0, 0, 1, 1, 0"),  // 10 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(3) = CLEARED
    line(4) = FILLED
    line(5) = CLEARED

    val solver = new NonogramSolverQueue(model, "", new ModelSolver(model))
    solver.splitter.splitByKnownChunk(line, metadata(0), BorderSolver)

    assert(solver.queue.size() === 2)
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(2), new LineImpl(0, Orientation.HORIZONTAL, model, 0, 4), BorderSolver))
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(2), new LineImpl(0, Orientation.HORIZONTAL, model, 5, 5), BorderSolver))
  }

  test("split by first chank") {
    val metadata = parseLine(Orientation.VERTICAL, "1 1 2")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0"),  // 11 cells
      metadata)

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)
    line(1) = CLEARED
    line(2) = FILLED
    line(3) = CLEARED
    line(4) = CLEARED
    line(5) = FILLED
    line(6) = CLEARED

    val solver = new NonogramSolverQueue(model, "", new ModelSolver(model))
    solver.splitter.splitByKnownChunk(line, metadata(0), BorderSolver)

    assert(solver.queue.size() === 2)
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(Array[Int]()), new LineImpl(0, Orientation.HORIZONTAL, model, 0, 2), BorderSolver))
    assert(solver.queue.take() === new SolveQueueTask(new LineMetadata(Array(1, 2)), new LineImpl(0, Orientation.HORIZONTAL, model, 3, 8), BorderSolver))
  }
}
