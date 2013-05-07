package ru.onehalf.japancrossword.model

/**
 * <p/>
 * <p/>
 * Created: 05.05.13 22:54
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordModel(val horizonLine : Array[Array[Int]], val verticalLine : Array[Array[Int]]) {

  val columnNumber = horizonLine.size
  val rowNumber = verticalLine.size

  private val board: Array[Array[Cell.Cell]] = Array.fill(columnNumber, rowNumber)(Cell.NOT_KNOWN)

  def getCell(x: Int, y: Int) = {
    board(x)(y)
  }

  def setCell(x: Int, y: Int, c: Cell.Cell) {
    board(x)(y) = c
  }

  def getColumn(x: Int) = getCell(x, _: Int)
  def getRow(y: Int) = getCell(_: Int, y)
}
