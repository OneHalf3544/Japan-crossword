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
class LineImpl(val lineIndex: Int, orientation: Orientation.Orientation,
           model: JapanCrosswordModel, fromIndex: Int, val size :Int) extends Line {

  def this(lineIndex: Int, orientation: Orientation.Orientation, model: JapanCrosswordModel, fromIndex: Int) =
    this(lineIndex, orientation, model, fromIndex, orientation match {
      case Orientation.HORIZONTAL => model.columnNumber - fromIndex
      case Orientation.VERTICAL => model.rowNumber - fromIndex
    })

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
    assert(cellIndex < size, "cellIndex " + cellIndex + " >= " + size)

    orientation match {
      case Orientation.HORIZONTAL => model(cellIndex + fromIndex, lineIndex) = cell
      case Orientation.VERTICAL => model(lineIndex, cellIndex + fromIndex) = cell
    }
  }

  val indexes = 0 to size - 1

  def forall(predicate: (Cell.Cell) => Boolean) = {
    indexes forall(cellIndex => predicate(apply(cellIndex)))
  }

  def drop(i: Int) = {
    new LineImpl(lineIndex, orientation, model, fromIndex + i, size -i)
  }

  def dropRight(i: Int) = {
    new LineImpl(lineIndex, orientation, model, fromIndex, size - i)
  }

  def nonEmpty() = {
    size != 0
  }

  def toList = {
    indexes.map(apply(_)).toList
  }

  def reverse(): Line = {
    new ReverseLine(this)
  }


  /**
   * Декоратор для имитации развернутого списка
   * @param original
   */
  class ReverseLine(original: Line) extends Line {

    override def reverse(): Line = {
      original
    }

    def lineIndex: Int = original.lineIndex

    def drop(i: Int): Line = {
      original.dropRight(i).reverse()
    }

    def dropRight(i: Int): Line = {
      original.drop(i).reverse()
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
