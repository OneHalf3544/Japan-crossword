package ru.onehalf.japancrossword

import model.{Cell, JapanCrosswordModel}

/**
 * <p/>
 * <p/>
 * Created: 07.05.13 7:28
 * <p/>
 * @author OneHalf
 */
class Solver(model: JapanCrosswordModel) {

  def fillLine(setCell: (Int, Cell.Cell) => Unit, lineLength: Int, metadata: Array[Int], getLineData: (Int) => Cell.Cell) {

    def addDataToModel(compromiss: List[Cell.Cell]) = {
      for (i <- 0 to lineLength-1) {
        val cell = getLineData(i)
        if (cell == Cell.NOT_KNOWN)
          setCell(i, compromiss(i))
        else
          setCell(i, cell)
      }
    }

    def reduceLines(line1: List[Cell.Cell], line2: List[Cell.Cell]): List[Cell.Cell] = {
      var result = List.fill[Cell.Cell](lineLength)(Cell.NOT_KNOWN)
      for (i <- 0 to lineLength -1) {
        if (line1(i) == line2(i))
          result = result.updated(i, line1(i))
        else
          result = result.updated(i,  Cell.NOT_KNOWN)
      }
      result
    }

    // Все возможные способы заполнения:
    val lines: Array[List[Cell.Cell]] = fitRemainder(lineLength, metadata)

    val compromiss: List[Cell.Cell] = lines.reduce(reduceLines)
    addDataToModel(compromiss)
  }

  def fillColumn(x: Int) {
    fillLine(model.setCell(x, _: Int, _: Cell.Cell), model.rowNumber, model.horizonLine(x), model.getCell(x, _:Int))
  }

  def fillRows(y: Int) {
    fillLine(model.setCell(_: Int, y, _: Cell.Cell), model.columnNumber, model.verticalLine(y), model.getCell(_:Int, y))
  }

  def fitRemainder(remainderCellCount: Int, remainder: Array[Int]): Array[List[Cell.Cell]] = {

    if (remainder.isEmpty) {
      return Array(List.fill[Cell.Cell](remainderCellCount)(Cell.CLEARED))
    }

    var lines: Array[List[Cell.Cell]] = Array.empty[List[Cell.Cell]]
    for (i <- 0 to remainderCellCount - 1) {

      val newRemainderCount = remainderCellCount - i - remainder(0)

      if (newRemainderCount >= 0) {
        var lineStart: List[Cell.Cell] =
          List.fill[Cell.Cell](i)(Cell.CLEARED) ::: List.fill[Cell.Cell](remainder(0))(Cell.FILLED)

        if (newRemainderCount > 0) {
          lineStart = lineStart ::: List.fill[Cell.Cell](1)(Cell.CLEARED)
        }

        fitRemainder(newRemainderCount - 1, remainder.tail).foreach(p => {lines = lines :+ (lineStart ::: p)})
      }
    }

    lines
  }
}
