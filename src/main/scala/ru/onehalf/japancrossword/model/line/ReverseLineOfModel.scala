package ru.onehalf.japancrossword.model.line

import ru.onehalf.japancrossword.model.Orientation.Orientation
import ru.onehalf.japancrossword.model.{Cell, Model}

/**
  * Декоратор для имитации развернутого списка
  *
  * @param original
  */
private[line] class ReverseLineOfModel(original: LineOfModel) extends LineOfModel {

  override private[line] val model: Model = original.model

  override private[line] def orientation: Orientation = original.orientation

  override private[line] val fromIndex: Int = original.fromIndex

  override def reverse(): LineOfModel = {
    original
  }

  def lineIndex: Int = original.lineIndex

  override def toList: List[Cell] = original.toList.reverse

  override def forall(predicate: Cell => Boolean): Boolean = original.forall(predicate)

  def size: Int = original.size

  def update(cellIndex: Int, cell: Cell) {
    original(size - cellIndex - 1) = cell
  }

  def apply(cellIndex: Int): Cell = {
    original(size - cellIndex - 1)
  }

  override def absoluteCoordinate(i: Int): (Int, Int) = original.absoluteCoordinate(size - i - 1)

  override def metadata: LineMetadata = original.metadata.reverse()

  override def ++(anotherLine: Line): Line = ??? // anotherLine.reverse() ++ this.reverse()

  override def dropLeft(metadataDropCount: Int, dropCount: Int): LineOfModel =
    original.dropRight(metadataDropCount, dropCount).reverse()

  override def dropRight(metadataDropCount: Int, dropCount: Int): LineOfModel =
    original.dropLeft(metadataDropCount, dropCount).reverse()

  override def dropClearedFromEnds(): LineOfModel = original.dropClearedFromEnds().reverse()
}
