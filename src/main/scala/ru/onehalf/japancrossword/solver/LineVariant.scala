package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{JapanCrosswordModel, Cell}

/**
 *
 * <p/>
 * <p/>
 * Created: 09.05.13 13:47
 * <p/>
 * @author OneHalf
 */
class LineVariant(lineIndex: Int, orientation: Orientation.Orientation,
                   var variant: List[Cell.Cell], model: JapanCrosswordModel) {

  def addDataToModel() {
    0 to variant.size-1 filter (getLineData(_) == Cell.NOT_KNOWN) foreach(i => setLineData(i, variant(i)))
  }

  def getLineData(cellIndex: Int) = {
    orientation match {
      case Orientation.HORIZONTAL => model(cellIndex, lineIndex)
      case Orientation.VERTICAL => model(lineIndex, cellIndex)
    }
  }

  def setLineData(cellIndex: Int, cell: Cell.Cell) {
    orientation match {
      case Orientation.HORIZONTAL => model.setCell(cellIndex, lineIndex, cell)
      case Orientation.VERTICAL => model.setCell(lineIndex, cellIndex, cell)
    }
  }
}
