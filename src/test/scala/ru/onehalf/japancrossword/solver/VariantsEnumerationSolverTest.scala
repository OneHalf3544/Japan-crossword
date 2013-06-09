package ru.onehalf.japancrossword.solver

import org.scalatest.concurrent.TimeLimits
import org.scalatest.{FlatSpec, Matchers}
import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.CrosswordLoader.parseLine
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.line._
import ru.onehalf.japancrossword.model.{Cell, Model}

import scala.collection.mutable

/**
 * <p/>
 * <p/>
 * Created: 12.05.13 15:31
 * <p/>
 * @author OneHalf
 */
class VariantsEnumerationSolverTest extends FlatSpec with Matchers with TimeLimits {

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK))
  val FILLED = new FilledCell(Color.BLACK)
  val CLEARED = Cleared

  it should "resolve center cells in line" in {

    val metadata = parseLine(Orientation.VERTICAL, "8")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 1, 1, 0, 0"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    val result = VariantsEnumerationSolver.fillLine(line)

    assert(result.size === 10)
    assert(result(0) === NOT_KNOWN)
    assert(result(1) === NOT_KNOWN)

    assert(2 to 7 forall(result(_) == FILLED))

    assert(result(8) === NOT_KNOWN)
    assert(result(9) === NOT_KNOWN)
  }

  it should "resolve all cells in line" in {

    val metadata = parseLine(Orientation.VERTICAL, "6")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 1, 1, 0, 0"),  // 10 cells
      metadata, Set(Color.BLACK))

    new ModelSolver(model).solve()
    Thread.sleep(100)
    assert(model.isSolved)
  }

  it should "set CLEARED cells status to model" in {

    val metadata = parseLine(Orientation.VERTICAL, "2 4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    line(1) = FILLED // Закрашиваем две клетки:          _X_______X
    line(9) = FILLED // После подбора строки должно быть _X_...XXXX

    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(
      NOT_KNOWN, FILLED, NOT_KNOWN, CLEARED, CLEARED,
      CLEARED, FILLED, FILLED, FILLED, FILLED))
  }

  it should "fill all line by Cell.FILLED status" in {

    val metadata = parseLine(Orientation.VERTICAL, "5")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "1, 1, 1, 1, 1"),  // 5 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(FILLED, FILLED, FILLED, FILLED, FILLED))
  }

  it should "fill space between two cells" in {

    val metadata = parseLine(Orientation.VERTICAL, "4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 0, 0"),  // 8 cells
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    // Выставляем две ячейки. Они должны быть соединены, т.к. должно быть только 4 ячейки идущих подряд
    line(3) = FILLED // Выставляем:     ___X_X__
    line(5) = FILLED // После решения:  .._XXX_.


    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(
      CLEARED, CLEARED,
      NOT_KNOWN,
      FILLED, FILLED, FILLED,
      NOT_KNOWN,
      CLEARED))
  }

  it should "sovle line by sublists" in {

    val metadata = parseLine(Orientation.VERTICAL, "4 4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0"),
      metadata, Set(Color.BLACK))

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    line(3) = FILLED
    line(5) = CLEARED
    line(7) = FILLED


    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(
      NOT_KNOWN, FILLED, FILLED, FILLED, NOT_KNOWN, CLEARED, NOT_KNOWN, FILLED, FILLED, FILLED, NOT_KNOWN))
  }

  "compatibleToCurrentData method" should "filter wrong variants" in {

    val line = new LineImpl(new LineMetadata(4), LineImpl.parse("_..X.X.."))

    def assertCompatible(list: Line) {
      assert(VariantsEnumerationSolver.compatibleToCurrentData(line, list), "line " + line + " not compatible to " + list)
    }

    def assertNotCompatible(list: Line) {
      assert(!VariantsEnumerationSolver.compatibleToCurrentData(line, list), "line " + line + " is compatible to " + list)
    }

    assertCompatible(new LineImpl(line.metadata, LineImpl.parse("..XXXX__")))
    assertCompatible(new LineImpl(line.metadata, LineImpl.parse("...XXX..")))
    assertCompatible(new LineImpl(line.metadata, LineImpl.parse("........")))

    assertNotCompatible(new LineImpl(line.metadata, LineImpl.parse("..XXX___")))
    assertNotCompatible(new LineImpl(line.metadata, LineImpl.parse("X.XXXX__")))
  }
}
