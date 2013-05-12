package ru.onehalf.japancrossword.model

/**
 * Модель японского кроссворда. Содержит метаданные и содержимое сетки
 * <p/>
 * <p/>
 * Created: 05.05.13 22:54
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordModel(val horizonLine : Metadata, val verticalLine : Metadata) {

  val columnNumber = horizonLine.size
  val rowNumber = verticalLine.size
  val maxTotalUnresolvedCount = columnNumber * rowNumber

  private var listeners: Array[()=>Unit] = Array()

  private var board: Array[Array[Cell.Cell]] = null

  clear()

  def clear() {
    board = Array.fill(columnNumber, rowNumber)(Cell.NOT_KNOWN)
    listeners.foreach(_())
  }

  def apply(x: Int, y: Int) = synchronized {
    board(x)(y)
  }

  def update(x: Int, y: Int, c: Cell.Cell) {
    synchronized[Unit] {
      board(x)(y) = c
    }
    listeners.foreach(_())
  }

  def getColumn(x: Int) = apply(x, _: Int)
  def getRow(y: Int) = apply(_: Int, y)

  def addListener(f :() => Unit) {
    listeners = listeners :+ f
  }

  def totalUnresolvedCount() = {
    board.map(_.filter(_ == Cell.NOT_KNOWN).size).sum
  }

  def isSolved = {
    totalUnresolvedCount() == 0
  }
}
