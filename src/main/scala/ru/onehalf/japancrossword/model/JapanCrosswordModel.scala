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
class JapanCrosswordModel(val horizonLine : Metadata, val verticalLine : Metadata) {

  private val readWriteLock = new ReentrantReadWriteLock()

  val columnNumber = horizonLine.size
  val rowNumber = verticalLine.size
  val maxTotalUnresolvedCount = columnNumber * rowNumber

  private var listeners: Array[()=>Unit] = Array()

  private var board: Array[Array[Cell.Cell]] = null

  clear()

  def clear() {
    writeAndNotify(() => board = Array.fill(columnNumber, rowNumber)(Cell.NOT_KNOWN))
  }

  def apply(x: Int, y: Int): Cell.Cell = {
    read[Cell.Cell](() => board(x)(y))
  }

  def update(x: Int, y: Int, c: Cell.Cell) {
    writeAndNotify(() =>  board(x)(y) = c)
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

  def read[T](func: () => T) = {
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
}
