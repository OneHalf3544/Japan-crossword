package ru.onehalf.japancrossword.model

import ru.onehalf.japancrossword.solver.Orientation

/**
 * Обертка над моделью для получения доступа к части данных как к массиву
 * <p/>
 * <p/>
 * Created: 09.05.13 13:47
 * <p/>
 * @author OneHalf
 */
class Line(lineIndex: Int, orientation: Orientation.Orientation, model: JapanCrosswordModel, fromIndex: Int) {

  def this(lineIndex: Int, orientation: Orientation.Orientation, model: JapanCrosswordModel) =
    this(lineIndex, orientation, model, 0)

  def apply(cellIndex: Int) = {
    orientation match {
      case Orientation.HORIZONTAL => model(cellIndex + fromIndex, lineIndex)
      case Orientation.VERTICAL => model(lineIndex, cellIndex + fromIndex)
    }
  }

  def update(cellIndex: Int, cell: Cell.Cell) {
    orientation match {
      case Orientation.HORIZONTAL => model(cellIndex + fromIndex, lineIndex) = cell
      case Orientation.VERTICAL => model(lineIndex, cellIndex + fromIndex) = cell
    }
  }

  val size = orientation match {
    case Orientation.HORIZONTAL => model.columnNumber - fromIndex
    case Orientation.VERTICAL => model.rowNumber - fromIndex
  }

  val indexes = 0 to size - 1

  def forall(predicate: (Cell.Cell) => Boolean) = {
    indexes forall(cellIndex => predicate(apply(cellIndex)))
  }

  def drop(i: Int) = {
    new Line(lineIndex, orientation, model, fromIndex + i)
  }

  def nonEmpty() = {
    size != 0
  }

  override def toString = {
    indexes.map(apply(_)).mkString("[", ", ", "]")
  }
}
