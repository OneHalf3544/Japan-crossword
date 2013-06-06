package ru.onehalf.japancrossword.model

import ru.onehalf.japancrossword.model.Cell._
import Orientation._

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 1:27
 * <p/>
 * @author OneHalf
 */
trait Line {

  def last: Cell.Cell = apply(size - 1)

  def lineIndex: Int

  def reverse(): Line

  def toList: List[Cell.Cell]

  def nonEmpty(): Boolean

  def drop(i: Int): Line

  def dropRight(i: Int): Line

  def forall(predicate: (Cell.Cell) => Boolean): Boolean

  def size: Int

  def orientation: Orientation

  def update(cellIndex: Int, cell: Cell.Cell)

  def apply(cellIndex: Int): Cell.Cell

  def absoluteCoordinate(i: Int): (Int, Int)

  def notKnownCount = (1 to size) count(i => apply(i - 1) == Cell.NOT_KNOWN)

  override def toString = "Line[%s]".format(toList.map({
    case FILLED => 'X'
    case CLEARED => '_'
    case NOT_KNOWN => '.'}).mkString)

  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Line]) {
      return false
    }

    val o = obj.asInstanceOf[Line]
    size == o.size && toList.corresponds(o.toList)((cell1, cell2) => cell1 == cell2)
  }
}
