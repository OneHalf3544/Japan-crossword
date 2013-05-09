package ru.onehalf.japancrossword

import model.{JapanCrosswordModel, Cell}
import solver.Orientation

/**
 * Варианты выставления
 * <p/>
 * <p/>
 * Created: 09.05.13 13:47
 * <p/>
 * @author OneHalf
 */
class LineVariants(lineIndex: Int, orientation: Orientation.Orientation,
                   var variants: Array[List[Cell.Cell]], model: JapanCrosswordModel) {

  variants = variants.filter(compatibleToCurrentData)

  /**
   * Схлопывание линий в одну. Если во всех вариантах какие-либо ячейки совпадают,
   * то результирующая линия будет содержать эти значения. Иначе в ячейку попадет Cell.NOT_KNOWN
   * @param line1 Линия 1
   * @param line2 Линия 2
   * @return Сезультат схлопывания
   */
  private def reduceLines(line1: List[Cell.Cell], line2: List[Cell.Cell]): List[Cell.Cell] = {
    assert(line1.size == line2.size)
    val lineLength = line1.size

    val result = Array.fill[Cell.Cell](lineLength)(Cell.NOT_KNOWN)

    0 to lineLength -1 filter ((i) => line1(i) == line2(i)) foreach(i => result(i) = line1(i))
    result.toList
  }

  /**
   * Проверка, не противоречит ли предлагаемое значение текущим данным
   * @param supposeLine Предлагаемая линия
   * @return true, если вариант приемлим
   */
  def compatibleToCurrentData(supposeLine: List[Cell.Cell]): Boolean = {
    0 to supposeLine.size-1 forall (i => getLineData(i) == Cell.NOT_KNOWN || getLineData(i) == supposeLine(i))
  }

  /**
   * Получение строки, которая не противоречит ни одному из вариантов.
   * Может содержать ячейки с неопределенным содержимым
   * @return Общий вариант расположения ячеек
   */
  def consistentVariant() = {
    variants.reduce(reduceLines)
  }

  def addDataToModel() {
    variants = variants.filter(compatibleToCurrentData)
    val newLineValue = consistentVariant()
    0 to newLineValue.size-1 filter (getLineData(_) == Cell.NOT_KNOWN) foreach(i => setLineData(i, newLineValue(i)))
  }

  def getLineData(cellIndex: Int) = {
    orientation match {
      case Orientation.HORIZONTAL => model.getCell(cellIndex, lineIndex)
      case Orientation.VERTICAL => model.getCell(lineIndex, cellIndex)
    }
  }

  def setLineData(cellIndex: Int, cell: Cell.Cell) {
    orientation match {
      case Orientation.HORIZONTAL => model.setCell(cellIndex, lineIndex, cell)
      case Orientation.VERTICAL => model.setCell(lineIndex, cellIndex, cell)
    }
  }
}
