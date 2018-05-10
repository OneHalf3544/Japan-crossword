package ru.onehalf.japancrossword.solver

import java.awt.Color

import com.typesafe.scalalogging.StrictLogging
import queue.{NonogramSolverQueue, SolveQueueTask}
import ru.onehalf.japancrossword.model.{Cell, Cleared, FilledCell}
import ru.onehalf.japancrossword.model.line.{Line, LineImpl, LineMetadata, LineOfModel}
import ru.onehalf.japancrossword.model.Cell._

import scala.collection.immutable

/**
  * Searches any subline what can be spit into two.
  *
  * It will improve performance - the sorter line we have the sooner we get a result.
  *
  * @since 26.05.13 22:45
  * @author OneHalf
  */
class LineSplitter(queue: NonogramSolverQueue) extends LineSolver with StrictLogging {

  private val clearedLine = new LineImpl(new LineMetadata(Array.emptyIntArray), Array(Cleared))

  // todo Делить линии по точно найденному участку.
  // Т.е.:  1 2 3  ......._XX_..... можно поделить по найденному участку независимо от того, что 2 != metadata.max

  override def fillLine(currentData: Line): Line = {throw new UnsupportedOperationException}

  def splitByKnownChunk(line: LineOfModel, solver: LineSolver): Boolean = {
    if (!line.contains(_.isFilled)) {
      return false
    }

    val stat: List[(Cell, Int)] = (Cleared, 0) +: line.countStat() :+ (Cleared, 0)
    val indexes: Seq[Int] = indicesForStat(stat)

    def searchBoundParts(statWindow: List[(Cell, Int)]): Option[Int] = {
      statWindow match {
        case List((Cleared, _), (FilledCell(Color.BLACK), chunkLength), (Cleared, _))  => Some(chunkLength)
        case _ => None
      }
    }

    val foundParts: Seq[Int] = stat.sliding(3)
      .map(searchBoundParts)
      .filter(_.isDefined)
      .map(_.get)
      .toList

    for (length <- foundParts) {
      // we can split line only if all chunk of this size already found.
      // Otherwise, we can't be sure in the correctness of the metadata splitting:
      if (foundParts.count(_ == length) == line.metadata.count(_ == length)) {
        val (line1: LineOfModel, line2: LineOfModel) = splitByFirstBoundChunk(line, stat, indexes, length)
        queue ! new SolveQueueTask(line1, solver)
        queue ! new SolveQueueTask(line2, solver)
        return true
      }
    }
    false
  }

  private def splitByFirstBoundChunk(line: LineOfModel,
                                     stat: List[(Cell, Int)],
                                     indexesForStat: Seq[Int],
                                     length: Int) = {
    val i = stat.indices
      .drop(1).dropRight(1)
      .filter(i => stat(i) == (FILLED, length) && stat(i - 1)._1 == Cleared && stat(i + 1)._1 == Cleared)
      .head

    val m = line.metadata.splitByFirstChunk(length)
    val m = line.metadata.splitAt(line.metadata.indexOf(length))
    val metadata1 = m._1
    val metadata2 = m._2.drop(1)

    val line1 = line.dropRight(metadata2.size + 1, line.size - indexesForStat(i))
    val line2 = line.dropLeft(metadata1.size + 1, indexesForStat(i + 1))

    logger.trace(s"splitted to: lines: $line1, $line2")
    (line1, line2)
  }

  /**
    * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
    *
    * @param currentData Текущие данные
    * @return true, если линия разделена
    */
  def splitLine(currentData: LineOfModel, solver: LineSolver): Boolean = {

    val metadata = currentData.metadata
    if (currentData(0) == Cleared || currentData.last == Cleared) {
      queue ! new SolveQueueTask(currentData.dropClearedFromEnds(), solver)
      return true
    }

    if (dropChanksFromEnds(currentData, solver)) {
      return true
    }

    if (currentData.metadata.size == 1) {
      return false
    }

    if (Line.countStat(currentData.toList.filterNot(_.isNotKnown)).count(_._1.isFilled) >= metadata.size) {
      val sublists = divideToSublists(currentData, currentData.countStat())
      assert(sublists.size == metadata.size, s"size not equals: $metadata and $sublists")
      sublists.indices foreach (v => queue ! new SolveQueueTask(sublists(v), solver))
      return true
    }

    if (splitByFirstMaxLength(currentData, solver)) {
      return true
    }

    splitByKnownChunk(currentData, solver)
  }

  /**
    * Отрезаем решенные кусочки от линии
    *
    * @param line Заполняемая линия
    * @param solver Решатель для строки
    * @return true, если строка была разделена
    */
  private[solver] def dropChanksFromEnds(line: LineOfModel, solver: LineSolver): Boolean = {
    val stat = line.countStat()

    if (stat.size <= 1) {
      return false
    }

    if (stat.head == (new FilledCell(Color.BLACK), line.metadata.head._1) && stat(1)._1 == Cleared) {
      queue ! new SolveQueueTask(line.dropLeft(1, line.metadata.head._1+1), solver)
      return true
    }

    if (stat.last == (new FilledCell(Color.BLACK), line.metadata.last._1) && stat(stat.size-2)._1 == Cleared) {
      queue ! new SolveQueueTask( line.dropRight(1, line.metadata.last._1+1), solver)
      return true
    }

    false
  }

  def splitByFirstMaxLength(line: LineOfModel, solver: LineSolver): Boolean = {
    def setClearedAt(i: Int) {
      if (i >= 0 && i < line.size) {
        line(i) = Cleared
      }
    }

    val stat = line.countStat()
    val indexes = indicesForStat(stat)
    val maxLength = line.metadata.max

    // if we don't find all chunks with max length, we cannot split line correctly
    if (line.metadata.count(_ == maxLength) != stat.count(_ == (new FilledCell(Color.BLACK), maxLength))) {
      return false
    }

    val indexOfFirstMaxChunk = stat.indices.filter(i => stat(i) == (new FilledCell(Color.BLACK), maxLength)).head
    // place cleared cells around the found chunk:
    setClearedAt(indexes(indexOfFirstMaxChunk) - 1)
    setClearedAt(indexes(indexOfFirstMaxChunk + 1))

    val m = line.metadata.splitByFirstChunk(maxLength)

    val line1 = line.dropRight(m._2.size, line.size - indexes(indexOfFirstMaxChunk))
    val line2 = line.dropLeft(m._1.size + 1, indexes(indexOfFirstMaxChunk + 1))

    logger.trace(s"splitted into: $line1, $line2")
    queue ! new SolveQueueTask(line1, solver)
    queue ! new SolveQueueTask(line2, solver)

    true
  }

  /**
    * Деление на подстроки.
    * Вызов метода подразумевает, что линия точно поделится на число линий равное metadata.size
    * @param line
    * @param stat
    * @return
    */
  def divideToSublists(line: LineOfModel, stat: List[(Cell, Int)]): List[LineOfModel] = {
    val statIndicies = indicesForStat(stat)

    /**
     *
     * @param i Индекс текущего найденого кусочка
     * @return true, если после укзанной позиции есть еще заполненные участки.
     *         (Используется для определения необходимости деления)
     */
    def isNotLastFilled(i: Int) = stat.drop(i+1).exists(_._1.isFilled)
    def isFilledBefore(i: Int) = stat.take(i).exists(_._1.isFilled)

    stat.indices.tail.find(i => { stat(i)._1 == Cleared && isFilledBefore(i) && isNotLastFilled(i)}) match {
      case Some(x) =>
        val splitIndex = statIndicies(x)
        val mc = line.metadata.size - 1
        line.dropRight(mc, line.size - splitIndex) +: divideToSublists(line.dropLeft(1, splitIndex), stat.drop(x))
      case None => List(line)
    }
  }

}
