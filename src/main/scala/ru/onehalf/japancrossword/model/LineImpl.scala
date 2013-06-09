package ru.onehalf.japancrossword.model

import Orientation._
import ru.onehalf.japancrossword.model.Cell.Cell

/**
 * Обертка над моделью для получения доступа к части данных как к массиву
 * <p/>
 * <p/>
 * Created: 09.05.13 13:47
 * <p/>
 * @author OneHalf
 */
class LineImpl(val lineIndex: Int, var orientation: Orientation,
           model: Model, fromIndex: Int, val size :Int) extends Line {

  def this(lineIndex: Int, orientation: Orientation, model: Model, fromIndex: Int) =
    this(lineIndex, orientation, model, fromIndex, orientation match {
      case HORIZONTAL => model.columnNumber - fromIndex
      case VERTICAL => model.rowNumber - fromIndex
    })

  def this(lineIndex: Int, orientation: Orientation, model: Model) =
    this(lineIndex, orientation, model, 0)

  def apply(cellIndex: Int): Cell = {
    assert(cellIndex < size)

    model(absoluteCoordinate(cellIndex))
  }

  def update(cellIndex: Int, cell: Cell) {
    assert(cellIndex < size, "cellIndex " + cellIndex + " >= " + size)

    model(absoluteCoordinate(cellIndex)) = cell
  }

  val indexes = 0 to size - 1

  def absoluteCoordinate(cellIndex: Int): (Int, Int) = {
    orientation match {
      case HORIZONTAL => (cellIndex + fromIndex, lineIndex)
      case VERTICAL => (lineIndex, cellIndex + fromIndex)
    }
  }

  def forall(predicate: (Cell) => Boolean) = {
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

    def toList: List[Cell] = original.toList.reverse

    def nonEmpty(): Boolean = original.nonEmpty()

    def forall(predicate: (Cell) => Boolean) = original.forall(predicate)

    def size: Int = original.size

    def update(cellIndex: Int, cell: Cell) {
      original(size - cellIndex - 1) = cell
    }

    def apply(cellIndex: Int): Cell = {
      original(size - cellIndex - 1)
    }

    def orientation: Orientation = {
      original.orientation
    }

    def absoluteCoordinate(i: Int): (Int, Int) = original.absoluteCoordinate(size - i - 1)
  }
}
