package ru.onehalf.japancrossword.solver

import com.typesafe.scalalogging.StrictLogging
import ru.onehalf.japancrossword.model.Cell.{Cell, _}
import ru.onehalf.japancrossword.model.line.LineMetadata.metadata
import ru.onehalf.japancrossword.model.line.{Line, LineImpl}

/**
  * Searches overlaps of the left and right positions to find filled cells.
  *
  * @since 12.05.13 22:51
  * @author OneHalf
  */
object SearchOverlapsSolver extends LineSolver with StrictLogging {

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   *
   * @param currentData Текущие данные
   */
  override def fillLine(currentData: Line): Line = {
    logger.trace(s"metadata: $currentData")
    assert(currentData.metadata.nonEmpty)

    def getSolution(filler: Line => Option[Line]): Line => Line =
      filler(_).getOrElse(throw new AssertionError(s"cannot find solution for $currentData"))


    val length = currentData.metadata.minimalLineLength
    assert(currentData.size >= length, "wrong length: " + length)

    val line1: Seq[Int] = numerateChunksInLine(currentData, getSolution(fitFromLeft))
    val line2: Seq[Int] = numerateChunksInLine(currentData, getSolution(fitFromRight))

    new LineImpl(
      currentData.metadata,
      (0 until currentData.size)
        .map {
          case i if line1(i) != 0 && line1(i) == line2(i) => FILLED
          case _ => NOT_KNOWN
        }
        .toArray)
  }

  private case class NCell(number: Int, isFilled: Boolean)

  /**
    * Converts a line to number sequence. Every digit means the number of a represented chunk.
    *
    * E.g., a call `numerateChunksInLine("[2, 3] | .......", fitFromLeft)` will become `[1, 1, 0, 2, 2, 2, 0, 0, 0]`
    *
    * @param currentData content of the line
    * @param fillLine a strategy to fill line with chunks (compact them either from the left or from the right).
    * @return
    */
  private[solver] def numerateChunksInLine(currentData: Line,
                                           fillLine: Line => Line): List[Int] = {
    fillLine(currentData).toList.scanLeft(NCell(0, isFilled = false))(numerateChunk)
      .drop(1)
      .map(v => if (v.isFilled) v.number else 0)
  }

  private def numerateChunk(pervious: NCell, cell: Cell) = cell match {
    // new chunk. increment a chunk number:
    case FILLED if !pervious.isFilled => NCell(pervious.number + 1, isFilled = true)
    // extend an existed chunk. use the same number:
    case FILLED if pervious.isFilled => NCell(pervious.number, isFilled = true)
    case CLEARED => NCell(pervious.number, isFilled = false)
    case NOT_KNOWN => throw new AssertionError(s"line shouldn't contain $NOT_KNOWN cell")
  }

  /** Заполнение линии согласно переданным в функцию метаданным.
    * Все кусочки создаются максимально смещенными влево
    *
    * @param currentData текущее содержимое линии
    * @return Линия, подходящих под указанные метаданные
    */
  private[solver] def fitFromLeft(currentData: Line): Option[Line] = {

    if (currentData.metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      return if (currentData.contains(FILLED)) {
        None
      } else {
        Some(LineImpl.empty(currentData.size))
      }
    }

    val chunkLength = currentData.metadata.head
    val chunk = List.fill[Cell](chunkLength)(new FilledCell(metadata.head._2))

    if (currentData.size == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return currentData.metadata.size match {
        case 1 => Some(new LineImpl(currentData.metadata, chunk))
        case _ => None
      }
    }

    // search the first possible position of the next chunk:
    for ( i <- 0 to maxChunkStartIndex(currentData)) {

      // Заполнение отступом + заполненный участок
      var lineArray: List[Cell] = List.fill(i)(Cleared) ++ chunk
      // Если какая-то часть строки еще остается, добавляем разделительную ячейку
      if (currentData.metadata.size > 1) {
        lineArray = lineArray :+ CLEARED
      }

      val lineImpl = new LineImpl(metadata(currentData.metadata.head), lineArray)

      if (currentData.canStartsWith(lineImpl)) {
        val restOfLine: Line = currentData.dropFromBegining(lineImpl)
        val result = fitFromLeft(restOfLine).map(lineImpl ++ _)
        if (result.isDefined) {
          return result
        }
      }

      // Продолжаем искать варианты
    }

    None
  }

  private def maxChunkStartIndex(currentData: Line): Int = {
    val firstExistedFilledCell = currentData.toList.indexOf(FILLED)
    val maxPossibleEmptySpace = currentData.size - currentData.metadata.minimalLineLength

    if (firstExistedFilledCell == -1)
      maxPossibleEmptySpace
    else
      Math.min(firstExistedFilledCell, maxPossibleEmptySpace)
  }

  def fitFromRight(currentData: Line): Option[Line] = fitFromLeft(currentData.reverse()).map(_.reverse())

}
