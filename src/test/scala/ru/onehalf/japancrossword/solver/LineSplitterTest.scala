package ru.onehalf.japancrossword.solver

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import ru.onehalf.japancrossword.CrosswordLoader._
import ru.onehalf.japancrossword.model.line._
import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.solver.queue.{NonogramSolverQueue, SolveQueueTask}

/**
 * <p/>
 * <p/>
 * Created: 21.05.13 8:49
 * <p/>
 * @author OneHalf
 */
class LineSplitterTest extends FlatSpec with MockFactory {

  behavior of "splitter"

  it should "divide line into three pieces" in {
    val model = oneLineModel("X__XXXX_XX")
    val line  =   createLine("X._..X._.X", model)

    val splitter = new LineSplitter(mock[NonogramSolverQueue])

    val result = splitter.divideToSublists(line, line.countStat())

    assert(result === List(
      new LineOfModelImpl(new LineMetadata(1), 0, Orientation.HORIZONTAL, model, 0, 2),
      new LineOfModelImpl(new LineMetadata(4), 0, Orientation.HORIZONTAL, model, 2, 5),
      new LineOfModelImpl(new LineMetadata(2), 0, Orientation.HORIZONTAL, model, 7, 3)))
  }

  it should "divide into two lines" in {

    val model = oneLineModel("___XXXX__X")
    val line  =   createLine("_._..X._.X", model)

    val solver = new LineSplitter(mock[NonogramSolverQueue])

    val result = solver.divideToSublists(line, line.countStat())

    assert(result === List(
      new LineOfModelImpl(new LineMetadata(4), 0, Orientation.HORIZONTAL, model, 0, 7),
      new LineOfModelImpl(new LineMetadata(1), 0, Orientation.HORIZONTAL, model, 7, 3)))

  }

  it should "divide the line into two pieces 2" in {

    val model = oneLineModel("_XX_XX____")
    val line  =   createLine(".X._.X.___", model)

    val solver = new LineSplitter(mock[NonogramSolverQueue])

    val result = solver.divideToSublists(line, line.countStat())

    assert(result === List(
      new LineOfModelImpl(new LineMetadata(2), 0, Orientation.HORIZONTAL, model, 0, 3),
      new LineOfModelImpl(new LineMetadata(2), 0, Orientation.HORIZONTAL, model, 3, 7)))

  }

  it should "split line by max part" in {
    val model = oneLineModel("_X__XX_X__")
    val line  =   createLine("..._XX_...", model)

    val solver = mock[NonogramSolverQueue]
    (solver ! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(1), 0, Orientation.HORIZONTAL, model, 0, 4), BorderSolver))
    (solver ! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(1), 0, Orientation.HORIZONTAL, model, 6, 4), BorderSolver))

    new LineSplitter(solver).splitByFirstMaxLength(line, BorderSolver)
  }

  it should "drop chunks from ends" in {
    val model = oneLineModel("XXX_XX_X__")
    val line  =   createLine("XXX_......", model)

    val solver = mock[NonogramSolverQueue]
    (solver.! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(2, 1), 0, Orientation.HORIZONTAL, model, 4, 6), BorderSolver))

    new LineSplitter(solver).dropChanksFromEnds(line, BorderSolver)
  }

  it should "split by known chunk" in {
    val model = oneLineModel("_XX_X__XX_")
    val line  =   createLine("..._X_....", model)

    val solver = mock[NonogramSolverQueue]
    (solver ! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(2), 0, Orientation.HORIZONTAL, model, 0, 4), BorderSolver))
    (solver ! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(2), 0, Orientation.HORIZONTAL, model, 5, 5), BorderSolver))

    new LineSplitter(solver).splitByKnownChunk(line, BorderSolver)
  }

  it should "split by first chunk" in {
    val model = oneLineModel("__X__X__XX_")
    val line  =   createLine("._X__X_....", model)

    val solverQueue = mock[NonogramSolverQueue]
    (solverQueue ! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(), 0, Orientation.HORIZONTAL, model, 0, 2), BorderSolver))
    (solverQueue ! _).expects(new SolveQueueTask(new LineOfModelImpl(new LineMetadata(1, 2), 0, Orientation.HORIZONTAL, model, 3, 8), BorderSolver))

    new LineSplitter(solverQueue).splitByKnownChunk(line, BorderSolver)
  }

  private def createLine(string: String, model: JapanCrosswordModel) = {
    val line = model.getRowLine(0)
    LineImpl.parse(string)
      .zipWithIndex
      .foreach(tuple => line(tuple._2) = tuple._1)

    line
  }

  private def oneLineModel(string: String) = {
    val solvedLine = createSolvedLine(string)
    val metadata = new ModelMetadata()(Orientation.VERTICAL, Array(solvedLine.metadata))
    val horizontalMetadata = solvedLine.toList
      .map(i => if (i == Cell.FILLED) 1 else 0)
      .map(LineMetadata.metadata(_))
      .toArray

    new Model("test",
      new Metadata(Orientation.HORIZONTAL, horizontalMetadata),
      metadata)
  }

  private def createSolvedLine(string: String) = {
    LineImpl.solved(LineImpl.parse(string): _*)
  }
}
