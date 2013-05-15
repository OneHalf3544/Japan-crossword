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
class Line(val lineIndex: Int, orientation: Orientation.Orientation, model: JapanCrosswordModel, fromIndex: Int) extends LineTrait {

  def this(lineIndex: Int, orientation: Orientation.Orientation, model: JapanCrosswordModel) =
    this(lineIndex, orientation, model, 0)

  def apply(cellIndex: Int) = {
    assert(cellIndex < size)

    orientation match {
      case Orientation.HORIZONTAL => model(cellIndex + fromIndex, lineIndex)
      case Orientation.VERTICAL => model(lineIndex, cellIndex + fromIndex)
    }
  }

  def update(cellIndex: Int, cell: Cell.Cell) {
    assert(cellIndex < size)

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

  def toList = {
    indexes.map(apply(_)).toList
  }

  def reverse(): LineTrait = {
    new ReverseLine(this)
  }

  class ReverseLine(original: LineTrait) extends LineTrait {

    override def reverse(): LineTrait = {
      original
    }

    def lineIndex: Int = original.lineIndex

    def drop(i: Int): LineTrait = {
      throw new UnsupportedOperationException("Not implemented yet")
    }

    def toList: List[Cell.Cell] = original.toList.reverse

    def nonEmpty(): Boolean = original.nonEmpty()

    def forall(predicate: (Cell.Cell) => Boolean) = original.forall(predicate)

    def size: Int = original.size

    def update(cellIndex: Int, cell: Cell.Cell) {
      original(size - cellIndex - 1) = cell
    }

    def apply(cellIndex: Int): Cell.Cell = {
      original(size - cellIndex - 1)
    }
  }
}
