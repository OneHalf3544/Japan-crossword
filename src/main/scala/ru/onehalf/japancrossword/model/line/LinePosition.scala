package ru.onehalf.japancrossword.model.line

import ru.onehalf.japancrossword.model.Cell.Cell
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import ru.onehalf.japancrossword.model.Orientation.{HORIZONTAL, Orientation, VERTICAL}

/** A position of a line inside a model.
  *
  * Date: 16.06.2018
  *
  * @author OneHalf
  */
class LinePosition(val orientation: Orientation,
                   val reversed: Boolean,
                   val fromIndex: Int,
                   val lineIndex: Int,
                   val size: Int) {
  def dropLeft(dropCount: Int): LinePosition = new LinePosition(
    orientation,
    reversed,
    if (reversed) fromIndex else fromIndex + dropCount,
    lineIndex,
    size - dropCount)

  def dropRight(dropCount: Int): LinePosition = new LinePosition(
    orientation,
    reversed,
    if (reversed) fromIndex + dropCount else fromIndex,
    lineIndex,
    size - dropCount)

  def absoluteCoordinate(cellIndex: Int): (Int, Int) = {
    val innerIndex = if (reversed) size - cellIndex - 1 else cellIndex
    orientation match {
      case HORIZONTAL => (innerIndex + fromIndex, lineIndex)
      case VERTICAL => (lineIndex, innerIndex + fromIndex)
    }
  }

  def reverse(): LinePosition = new LinePosition(orientation, !reversed, fromIndex, lineIndex, size)
}
