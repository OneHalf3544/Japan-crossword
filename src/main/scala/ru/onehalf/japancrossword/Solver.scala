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

  def fillLine(setCell: (Int, Cell.Cell) => Unit, lineLength: Int, metadata: Array[Int]) {
    // Все возможные способы заполнения:
    val line: Array[List[Cell.Cell]] = fitRemainder(lineLength, metadata)

    for (i <- 1 to lineLength) {
      if (!line.isEmpty) {
        setCell(i-1, line(0)(i-1))
      }
    }
  }

  def fillColumn(x: Int) {
    fillLine(model.setCell(x, _: Int, _: Cell.Cell), model.rowNumber, model.horizonLine(x))
  }

  def fillRows(y: Int) {
    fillLine(model.setCell(_: Int, y, _: Cell.Cell), model.columnNumber, model.verticalLine(y))
  }

  def fitRemainder(remainderCellCount: Int, remainder: Array[Int]): Array[List[Cell.Cell]] = {
    println(remainderCellCount)
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

    //println(lines(0))
    lines
  }
}
