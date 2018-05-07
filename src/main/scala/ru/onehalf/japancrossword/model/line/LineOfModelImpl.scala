package ru.onehalf.japancrossword.model.line

import ru.onehalf.japancrossword.model.Cell.Cell
import ru.onehalf.japancrossword.model.Orientation.{HORIZONTAL, Orientation, VERTICAL}
import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel}

/**
  * Обертка над моделью для получения доступа к части данных как к массиву.
  *
  * @since 09.05.13 13:47
  * @author OneHalf
  */
class LineOfModelImpl(override val metadata: LineMetadata,
                      override val lineIndex: Int,
                      override val orientation: Orientation,
                      override val model: JapanCrosswordModel,
                      override val fromIndex: Int,
                      override val size :Int) extends LineOfModel {

  def this(lineMetadata: LineMetadata,
           lineIndex: Int,
           orientation: Orientation,
           model: JapanCrosswordModel,
           fromIndex: Int) =
    this(lineMetadata, lineIndex, orientation, model, fromIndex, orientation match {
      case HORIZONTAL => model.columnNumber - fromIndex
      case VERTICAL => model.rowNumber - fromIndex
    })

  def this(lineMetadata: LineMetadata,
           lineIndex: Int,
           orientation: Orientation,
           model: JapanCrosswordModel) =
    this(lineMetadata, lineIndex, orientation, model, 0)

  override def apply(cellIndex: Int): Cell = {
    assert(cellIndex < size, s"expected cellIndex ($cellIndex) < size ($size)")

    model(absoluteCoordinate(cellIndex))
  }

  override def update(cellIndex: Int, cell: Cell) {
    assert(cellIndex < size, "cellIndex " + cellIndex + " >= " + size)

    model.update(absoluteCoordinate(cellIndex), cell)
  }

  override def absoluteCoordinate(cellIndex: Int): (Int, Int) = {
    orientation match {
      case HORIZONTAL => (cellIndex + fromIndex, lineIndex)
      case VERTICAL => (lineIndex, cellIndex + fromIndex)
    }
  }

  override def toList: List[Cell] = {
    indexes.map(apply).toList
  }

  override def reverse(): LineOfModel = {
    new ReverseLineOfModel(this)
  }

  override def ++(anotherLine: Line): Line = throw new UnsupportedOperationException

  override def dropLeft(metadataDropCount: Int, dropCount: Int): LineOfModel = new LineOfModelImpl(
    new LineMetadata(metadata.drop(metadataDropCount)),
    lineIndex, orientation, model, fromIndex + dropCount, size - dropCount
  )

  override def dropRight(metadataDropCount: Int, dropCount: Int): LineOfModel = new LineOfModelImpl(
    new LineMetadata(metadata.take(metadata.size - metadataDropCount)),
    lineIndex, orientation, model, fromIndex, size - dropCount
  )
}
