package ru.onehalf.japancrossword.model


/**
 * <p/>
 * <p/>
 * Created: 15.05.13 1:27
 * <p/>
 * @author OneHalf
 */
trait LineTrait {

  def lineIndex: Int

  def reverse(): LineTrait

  def toList: List[Cell.Cell]

  def nonEmpty(): Boolean

  def drop(i: Int): LineTrait

  def dropRight(i: Int): LineTrait

  def forall(predicate: (Cell.Cell) => Boolean): Boolean

  def size: Int

  def update(cellIndex: Int, cell: Cell.Cell)

  def apply(cellIndex: Int): Cell.Cell

  override def toString = {
    toList.mkString("[", ", ", "]")
  }

}
