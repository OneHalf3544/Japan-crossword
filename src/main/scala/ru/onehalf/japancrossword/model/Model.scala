package ru.onehalf.japancrossword.model

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.awt.Color

/**
 * A nonogram model. Contains of two [[Metadata]] and grid content.
 * <p/>
 * <p/>
 * Created: 05.05.13 22:54
 * <p/>
 * @author OneHalf
 */
class Model(val name: String, val horizonLine : ModelMetadata, val verticalLine : ModelMetadata, val colors: Set[Color]) {

  def this(name: String, horizonLine : ModelMetadata, verticalLine : ModelMetadata) =
    this(name, horizonLine, verticalLine, Set(Color.BLACK))

  private val readWriteLock = new ReentrantReadWriteLock()

  val columnNumber: Int = horizonLine.size
  val rowNumber: Int = verticalLine.size
  val maxTotalUnresolvedCount: Int = columnNumber * rowNumber

  private var listeners: List[()=>Unit] = List()

  private var board: Array[Array[Cell]] = _

  clear()

  def clear() {
    writeAndNotify(() => board = Array.fill(columnNumber, rowNumber)(new NotKnownCell(colors)))
  }

  def apply(coordinate: (Int, Int)): Cell = {
    apply(coordinate._1, coordinate._2)
  }

  def apply(x: Int, y: Int): Cell = {
    read(() => board(x)(y))
  }

  def update(x: Int, y: Int, c: Cell) {
    writeAndNotify(() =>  board(x)(y) = c)
  }

  def update(coordinate: (Int, Int), c: Cell) {
    this(coordinate._1, coordinate._2) = c
  }

  def getColumn(x: Int) = apply(x, _: Int)

  def getRow(y: Int) = apply(_: Int, y)

  def getRowLine(i: Int): (Line, LineMetadata) = {
    (new LineImpl(i, Orientation.HORIZONTAL, this), verticalLine(i))
  }

  def getColumnLine(i: Int): (Line, LineMetadata) = {
    (new LineImpl(i, Orientation.VERTICAL, this), horizonLine(i))
  }

  def addListener(f :() => Unit) {
    listeners = listeners :+ f
  }

  def removeListener(f :() => Unit) {
    listeners = listeners.filterNot(_ == f)
  }

  def totalUnresolvedCount(): Int = {
    board.map(_.count(_.isNotKnown)).sum
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
