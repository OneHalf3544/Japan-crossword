package ru.onehalf.japancrossword.solver

import org.scalatest.concurrent.TimeLimits
import org.scalatest.{FlatSpec, Matchers}
import ru.onehalf.japancrossword.model.{Orientation, Cell, LineImpl, Model}
import ru.onehalf.japancrossword.model.Cell._
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

  it should "resolve center cells in line" in {

    val metadata = parseLine(Orientation.VERTICAL, "8")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 1, 1, 0, 0"),  // 10 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    val result = VariantsEnumerationSolver.fillLine(line)

    assert(result.size === 10)
    assert(result(0) === Cell.NOT_KNOWN)
    assert(result(1) === Cell.NOT_KNOWN)

    assert(2 to 7 forall(result(_) == Cell.FILLED))

    assert(result(8) === Cell.NOT_KNOWN)
    assert(result(9) === Cell.NOT_KNOWN)
  }

  it should "resolve all cells in line" in {

    val metadata = parseLine(Orientation.VERTICAL, "6")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 1, 1, 0, 0"),  // 10 cells
      metadata)

    new ModelSolver(model).solve()
    Thread.sleep(100)
    assert(model.isSolved)
  }

  it should "set CLEARED cells status to model" in {

    val metadata = parseLine(Orientation.VERTICAL, "2 4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    line(1) = Cell.FILLED // Закрашиваем две клетки:          _X_______X
    line(9) = Cell.FILLED // После подбора строки должно быть _X_...XXXX

    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(
      Cell.NOT_KNOWN, Cell.FILLED, Cell.NOT_KNOWN, Cell.CLEARED, Cell.CLEARED,
      Cell.CLEARED, Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED))
  }

  it should "fill all line by Cell.FILLED status" in {

    val metadata = parseLine(Orientation.VERTICAL, "5")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "1, 1, 1, 1, 1"),  // 5 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED, Cell.FILLED))
  }

  it should "fill space between two cells" in {

    val metadata = parseLine(Orientation.VERTICAL, "4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 0, 1, 1, 1, 1, 0, 0"),  // 8 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    // Выставляем две ячейки. Они должны быть соединены, т.к. должно быть только 4 ячейки идущих подряд
    line(3) = Cell.FILLED // Выставляем:     ___X_X__
    line(5) = Cell.FILLED // После решения:  .._XXX_.


    val result = VariantsEnumerationSolver.fillLine(line).toList

    assert(result === List(
      Cell.CLEARED, Cell.CLEARED,
      Cell.NOT_KNOWN,
      Cell.FILLED, Cell.FILLED, Cell.FILLED,
      Cell.NOT_KNOWN,
      Cell.CLEARED))
  }

  it should "sovle line by sublists" in {

    val metadata = parseLine(Orientation.VERTICAL, "4 4")
    val model = new Model("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0"),
      metadata)

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
