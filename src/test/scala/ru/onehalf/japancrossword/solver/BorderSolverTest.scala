package ru.onehalf.japancrossword.solver

import org.scalatest.{FlatSpec, Matchers}
import ru.onehalf.japancrossword.model._
import java.awt.Color
import ru.onehalf.japancrossword.CrosswordLoader

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 2:10
 * <p/>
 * @author OneHalf
 */
class BorderSolverTest extends FlatSpec with Matchers {

  val NOT_KNOWN = new NotKnownCell(Set(Color.BLACK), true)
  val FILLED = new FilledCell(Color.BLACK)

  it should "set CLEARED cells status to model" in {

    val metadata = CrosswordLoader.parseLine(Orientation.VERTICAL, "2 4")
    val model = new Model("test",
      CrosswordLoader.parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 0, 0, 1, 1, 1, 1"),  // 10 cells
      metadata, Set(Color.BLACK))

    val line = new LineImpl(0, Orientation.HORIZONTAL, model)

    line(1) = FILLED // Закрашиваем две клетки:          _X_______X
    line(9) = FILLED // После подбора строки должно быть _X_...XXXX

    val result = VariantsEnumerationSolver.fitRemainder(metadata(0), line).get

    assert(result === List(
      NOT_KNOWN, FILLED, NOT_KNOWN, Cleared, Cleared,
      Cleared, FILLED, FILLED, FILLED, FILLED))
  }

}
