package ru.onehalf.japancrossword.model.line

import ru.onehalf.japancrossword.model.Orientation.{HORIZONTAL, Orientation, VERTICAL}
import ru.onehalf.japancrossword.model.{Cell, Model}

/**
  * Обертка над моделью для получения доступа к части данных как к массиву.
  *
  * @since 09.05.13 13:47
  * @author OneHalf
  */
trait LineOfModel extends Line {

  private[line] val model: Model
  private[line] def orientation: Orientation
  private[line] val fromIndex: Int

  def absoluteCoordinate(cellIndex: Int): (Int, Int) = {
    orientation match {
      case HORIZONTAL => (cellIndex + fromIndex, lineIndex)
      case VERTICAL => (lineIndex, cellIndex + fromIndex)
    }
  }

  def lineIndex: Int

  def toList: List[Cell] = {
    indexes.map(apply).toList
  }

  def reverse(): LineOfModel

  override def ++(anotherLine: Line): Line = throw new UnsupportedOperationException

  override def dropLeft(metadataDropCount: Int, dropCount: Int): LineOfModel

  override def dropRight(metadataDropCount: Int, dropCount: Int): LineOfModel

  override def dropClearedFromEnds(): LineOfModel = super.dropClearedFromEnds().asInstanceOf[LineOfModel]
}
