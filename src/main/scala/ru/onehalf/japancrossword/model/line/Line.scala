package ru.onehalf.japancrossword.model.line

import ru.onehalf.japancrossword.model.Cell
import ru.onehalf.japancrossword.model.Cell._

/**
  * One of nonogram line.
  * Contains corresponding metadata and line content
  * <p>
  * @since 15.05.13 1:27
  * @author OneHalf
  */
trait Line {

  def contains(cell: Cell): Boolean = !forall(_ != cell)

  def ++(anotherLine: Line): Line

  val indexes: Range = 0 until size

  def dropLeft(metadataDropCount: Int, dropCount: Int): Line

  def dropRight(metadataDropCount: Int, dropCount: Int): Line

  def canStartsWith(lineStart: Line): Boolean = size >= lineStart.size && (0 until lineStart.size)
    .forall(i => {
      val cell1 = apply(i)
      val cell2 = lineStart(i)
      cell1 == Cell.NOT_KNOWN || cell2 == Cell.NOT_KNOWN || cell1 == cell2
    })

  def dropFromBegining(lineStart: Line): Line = {
    assert(this.canStartsWith(lineStart), s"$this do not starts from $lineStart")
    dropLeft(lineStart.metadata.size, lineStart.size)
  }

  /**
    * Removes [[Cell.CLEARED]] cells from the ends of line
    *
    * @return new line
    */
  def dropClearedFromEnds(): Line = {
    if (isEmpty) {
      return this
    }
      if (apply(0) == Cell.CLEARED) {
      return dropLeft(0, 1).dropClearedFromEnds()
    }
      if (last == Cell.CLEARED) {
      return dropRight(0, 1).dropClearedFromEnds()
    }
    this
  }

  def metadata: LineMetadata

  def last: Cell.Cell = apply(size - 1)

  def reverse(): Line

  def toList: List[Cell.Cell]

  def isEmpty: Boolean = size == 0

  def nonEmpty(): Boolean = !isEmpty

  def forall(predicate: Cell => Boolean): Boolean = {
    indexes forall(cellIndex => predicate(apply(cellIndex)))
  }

  def size: Int

  def apply(cellIndex: Int): Cell.Cell

  def update(cellIndex: Int, cell: Cell)

  def notKnownCount: Int = indexes count(apply(_) == Cell.NOT_KNOWN)

  def isSolved: Boolean = notKnownCount == 0

  /**
    * * Compressed info about cell sequence.
    * * <p>
    * * For example , "_XX_..X." will become {{{List(
    *     *   (Cell.CLEARED, 1),
    *     *   (Cell.FILLED, 2),
    *     *   (Cell.CLEARED, 1),
    *     *   (Cell.NOT_KNOWN, 2),
    *     *   (Cell.FILLED, 1),
    *     *   (Cell.NOT_KNOWN, 1)
    *     * )}}}
    *
    * @return
    */
  def countStat(): List[(Cell, Int)] = Line.countStat(this.toList)

  override def toString: String = "%s | %s".format(metadata, toList.map({
    case FILLED => 'X'
    case CLEARED => '_'
    case NOT_KNOWN => '.'}).mkString)

  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Line]) {
      return false
    }

    val o = obj.asInstanceOf[Line]
    size == o.size && toList == o.toList && metadata == o.metadata
  }
}

object Line {
  /**
    *
    * @param currentData
    * @return
    */
  def countStat(currentData: Traversable[Cell]): List[(Cell, Int)] = {
    currentData.foldLeft(List.empty[(Cell, Int)])(countCellTypes)
  }

  /**
    * Adds info about a cell into {{{accumulator}}}.
    *
    * @param accumulator accumulator to save info
    * @param nextCell cell to adding
    * @return info about cells sequence
    */
  private def countCellTypes(accumulator: List[(Cell, Int)], nextCell: Cell): List[(Cell, Int)] = {
    if (accumulator.isEmpty) {
      // it's a first cell
      return List((nextCell, 1))
    }
    if (accumulator.last._1 == nextCell) {
      // replace last value with incremented last counter if next cell is is the same type as previous
      return accumulator.init :+ (nextCell, accumulator.last._2 + 1)
    }
    // add new value if cell type differs from previous
    accumulator :+ (nextCell, 1)
  }
}