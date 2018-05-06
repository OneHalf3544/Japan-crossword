package ru.onehalf.japancrossword.model

import java.util.concurrent.locks.ReentrantReadWriteLock

/**
 * Модель японского кроссворда. Содержит метаданные и содержимое сетки
 * <p/>
 * <p/>
 * Created: 05.05.13 22:54
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordModel(val name: String, val horizonLine : Metadata, val verticalLine : Metadata) {

  private val readWriteLock = new ReentrantReadWriteLock()

  val columnNumber: Int = horizonLine.size
  val rowNumber: Int = verticalLine.size
  val maxTotalUnresolvedCount: Int = columnNumber * rowNumber

  private var listeners: List[()=>Unit] = List()

  private var board: Array[Array[Cell.Cell]] = _

  clear()

  def clear() {
    writeAndNotify(() => board = Array.fill(columnNumber, rowNumber)(Cell.NOT_KNOWN))
  }

  def apply(coordinate: (Int, Int)): Cell.Cell = {
    apply(coordinate._1, coordinate._2)
  }

  def apply(x: Int, y: Int): Cell.Cell = {
    read(() => board(x)(y))
  }

  def update(x: Int, y: Int, c: Cell.Cell) {
    writeAndNotify(() =>  board(x)(y) = c)
  }

  def update(coordinate: (Int, Int), c: Cell.Cell) {
    this(coordinate._1, coordinate._2) = c
  }

  def getColumn(x: Int) = apply(x, _: Int)

  def getRow(y: Int) = apply(_: Int, y)

  def getRowLine(i: Int): (Line, Array[Int]) = {
    (new LineImpl(i, Orientation.HORIZONTAL, this), verticalLine(i))
  }

  def getColumnLine(i: Int): (Line, Array[Int]) = {
    (new LineImpl(i, Orientation.VERTICAL, this), horizonLine(i))
  }

  def addListener(f :() => Unit) {
    listeners = listeners :+ f
  }

  def removeListener(f :() => Unit) {
    listeners = listeners.filterNot(_ == f)
  }

  def totalUnresolvedCount(): Int = {
    board.map(_.count(_ == Cell.NOT_KNOWN)).sum
  }

  def isSolved: Boolean = {
    totalUnresolvedCount() == 0
  }

  def read[T](func: () => T): T = {
    readWriteLock.readLock().lock()
    try
      func()

    finally readWriteLock.readLock().unlock()
  }

  def writeAndNotify(func: () => Unit) {
    readWriteLock.writeLock().lock()
    try
      func()

    finally readWriteLock.writeLock().unlock()
    listeners.foreach(_())
  }

  override def toString: String = name + " (" + horizonLine.size + "x" + verticalLine.size + ")"

}
