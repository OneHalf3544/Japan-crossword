package ru.onehalf.japancrossword.model

import Orientation._

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 1:27
 * <p/>
 * @author OneHalf
 */
trait Line {

  def exists(predicate: (Cell) => Boolean): Boolean = {
    ! forall(! predicate(_))
  }

  def last: Cell = apply(size - 1)

  def lineIndex: Int

  def reverse(): Line

  def toList: List[Cell]

  def nonEmpty(): Boolean

  def drop(i: Int): Line

  def dropRight(i: Int): Line

  def forall(predicate: Cell => Boolean): Boolean

  def size: Int

  def orientation: Orientation

  def update(cellIndex: Int, cell: Cell)

  def apply(cellIndex: Int): Cell

  def absoluteCoordinate(i: Int): (Int, Int)

  def notKnownCount: Int = (1 to size) count(i => apply(i - 1).isNotKnown)

  override def toString: String = "Line[%s]".format(toList.map({
    case FilledCell(_) => 'X'
    case Cleared => '_'
    case NotKnownCell(_) => '.'}).mkString)

  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Line]) {
      return false
    }

    val o = obj.asInstanceOf[Line]
    size == o.size && toList == o.toList
  }
}
