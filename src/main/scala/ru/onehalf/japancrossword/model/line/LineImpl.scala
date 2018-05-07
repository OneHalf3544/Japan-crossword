package ru.onehalf.japancrossword.model.line
import java.util.stream

import ru.onehalf.japancrossword.model
import ru.onehalf.japancrossword.model.Cell
import ru.onehalf.japancrossword.model.Cell.{CLEARED, Cell, FILLED, NOT_KNOWN}
import ru.onehalf.japancrossword.model.Orientation.Orientation
import ru.onehalf.japancrossword.model.line.LineMetadata.metadata

class LineImpl(override val metadata: LineMetadata, val array: Array[Cell]) extends Line {
  assert(
    metadata.minimalLineLength <= array.length,
    s"cannot create line: metadata $metadata requires a longer content than ${array.length}")

  def this(metadata: LineMetadata, list: List[Cell]) = this(metadata, list.toArray)

  override def dropClearedFromEnds(): Line = new LineImpl(
    metadata,
    array.dropWhile(_ == CLEARED).reverse.dropWhile(_ == CLEARED).reverse)

  override def reverse(): Line = new LineImpl(metadata.reverse(), array.reverse)

  override def toList: List[Cell] = array.toList

  override def nonEmpty(): Boolean = array.nonEmpty

  override def forall(predicate: Cell => Boolean): Boolean = array.forall(predicate)

  override def size: Int = array.length

  override def update(cellIndex: Int, cell: Cell) {
    array(cellIndex) = cell
  }

  override def apply(cellIndex: Int): Cell = array(cellIndex)

  override def ++(anotherLine: Line): Line = new LineImpl(
    new LineMetadata(metadata ++ anotherLine.metadata),
    array ++ anotherLine.toList)

  override def dropLeft(metadataDropCount: Int, dropCount: Int): Line =
    new LineImpl(
      new LineMetadata(metadata.drop(metadataDropCount)),
      array.drop(dropCount)
    )

  override def dropRight(metadataDropCount: Int, dropCount: Int): Line =
    reverse().dropLeft(metadataDropCount, dropCount).reverse()
}

object LineImpl {

  def parse(string: String): List[Cell] = {
    string
      .toCharArray
      .map {
        case 'X' => Cell.FILLED
        case '_' => Cell.CLEARED
        case _ => Cell.NOT_KNOWN
      }
      .toList
  }

  def solved(cells: Cell*): Line = {
    assert(!cells.contains(NOT_KNOWN))
    new LineImpl(
      new LineMetadata(Line.countStat(cells).filter(_._1 == FILLED).map(_._2)),
      cells.toList
    )
  }


  def empty(size: Int): LineImpl = {
    new LineImpl(LineMetadata.empty(), Array.fill(size)(CLEARED))
  }

  def parseLine(metadata: LineMetadata, string: String): LineImpl = {
    new LineImpl(metadata, parse(string))
  }

  def parseLine(int1: Int, string: String): LineImpl = {
    new LineImpl(metadata(int1), parse(string))
  }

  def parseLine(int1: Int, int2: Int, string: String): LineImpl = {
    new LineImpl(metadata(int1, int2), parse(string))
  }

  def parseLine(int1: Int, int2: Int, int3: Int, string: String): LineImpl = {
    new LineImpl(metadata(int1, int2, int3), parse(string))
  }

  def parseLine(int1: Int, int2: Int, int3: Int, int4: Int, string: String): LineImpl = {
    new LineImpl(metadata(int1, int2, int3, int4), parse(string))
  }
}
