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

  private var listeners: Array[()=>Unit] = Array()

  private var board: Array[Array[Cell.Cell]] = null

  clear()

  def clear() {
    board = Array.fill(columnNumber, rowNumber)(Cell.NOT_KNOWN)
    listeners.foreach(_())
  }

  def getCell(x: Int, y: Int) = {
    board(x)(y)
  }

  def setCell(x: Int, y: Int, c: Cell.Cell) {
    board(x)(y) = c
    listeners.foreach(_())
  }

  def getColumn(x: Int) = getCell(x, _: Int)
  def getRow(y: Int) = getCell(_: Int, y)

  def addListener(f :() => Unit) {
    listeners = listeners :+ f
  }
}
