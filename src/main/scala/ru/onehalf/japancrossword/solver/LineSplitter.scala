package ru.onehalf.japancrossword.solver

import queue.{NonogramSolverQueue, SolveQueueTask}
import ru.onehalf.japancrossword.model._
import java.awt.Color
import scala.Some
import ru.onehalf.japancrossword.model.FilledCell

/**
  * Searches any subline what can be spit into two.
  *
  * It will improve performance - the sorter line we have the sooner we get a result.
  *
  * @since 26.05.13 22:45
  * @author OneHalf
  */
class LineSplitter(queue: NonogramSolverQueue) extends LineSolver {

  // todo Делить линии по точно найденному участку.
  // Т.е.:  1 2 3  ......._XX_..... можно поделить по найденному участку независимо от того, что 2 != metadata.max

  def fillLine(metadata: LineMetadata, currentData: Line): List[Cell] = {throw new UnsupportedOperationException}

  def dropClearedFromEnds(line: Line): Line = {
    if (line(0) == Cleared) {
      return dropClearedFromEnds(line.drop(1))
    }
    if (line.last == Cleared) {
      return dropClearedFromEnds(line.dropRight(1))
    }
    line
  }

  def splitByKnownChunk(line: Line, metadata: LineMetadata, solver: LineSolver): Boolean = {
    val stat: List[(Cell, Int)] = (Cleared, 0) +: countStat(line) :+ (Cleared, 0)
    val indexes = indicesForStat(stat)

    def searchBoundedParts(l: List[(Cell, Int)]): Option[Int] = {
      l match {
        case List((Cleared, _), (FilledCell(Color.BLACK), chunkLength), (Cleared, _))  => Some(chunkLength)
        case _ => None
      }
    }

    val foundedParts = stat.sliding(3).map(searchBoundedParts).filter(_.isDefined).map(_.get).toList

    if (foundedParts.isEmpty) {
      return false
    }

    for (length <- foundedParts) {
      if (foundedParts.count(_ == length) == metadata.count(_ == length)) {
        val i = stat.indices.init.tail.filter(i => stat(i) == (FilledCell(Color.BLACK), length) && stat(i - 1)._1 == Cleared && stat(i + 1)._1 == Cleared ).head

        val m = metadata.splitByFirstChunk(length)

        val metadata1 = m._1
        val line1 = line.dropRight(line.size - indexes(i))

        val metadata2 = m._2.drop(1)
        val line2 = line.drop(indexes(i + 1))

        //println("splitted to: %s,%s, lines: %s, %s".format(metadata1, metadata2, line1, line2))
        queue ! new SolveQueueTask(metadata1, line1, solver)
        queue ! new SolveQueueTask(metadata2, line2, solver)

        return true
      }
    }
    false
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return true, если линия разделена
   */
  def splitLine(metadata: LineMetadata, currentData: Line, solver: LineSolver): Boolean = {

    if (true) {
      return false
    }

    if (currentData(0) == Cleared || currentData.last == Cleared) {
      queue ! new SolveQueueTask(metadata, dropClearedFromEnds(currentData), solver)
      return true
    }

    if (dropChanksFromEnds(metadata, currentData, solver)) {
      return true
    }

    if (metadata.size == 1) {
      return false
    }

    if (countStat(currentData.toList.filterNot(_.isNotKnown)).count(_._1.isFilled) >= metadata.size) {
      val sublists = divideToSublists(currentData, countStat(currentData))
      assert(sublists.size == metadata.size, "size not equals: %s and %s".format(metadata, sublists))
      sublists.indices map (i => queue ! new SolveQueueTask(new LineMetadata(metadata(i)), sublists(i), solver))
      return true
    }

    if (splitByFirstMaxLength(currentData, metadata, solver)) {
      return true
    }

    splitByKnownChunk(currentData, metadata, solver)
  }

  /**
   * Отрезаем решенные кусочки от линии
   * @param metadata Метаданные
   * @param line Заполняемая линия
   * @param solver Решатель для строки
   * @return true, если строка была разделена
   */
  def dropChanksFromEnds(metadata: LineMetadata, line: Line, solver: LineSolver): Boolean = {
    val stat = countStat(line)

    if (stat.size <= 1) {
      return false
    }

    if (stat.head == (new FilledCell(Color.BLACK), metadata.head._1) && stat(1)._1 == Cleared) {
      queue ! new SolveQueueTask(metadata.tail, line.drop(metadata.head._1 + 1), solver)
      return true
    }

    if (stat.last == (new FilledCell(Color.BLACK), metadata.last._1) && stat(stat.size-2)._1 == Cleared) {
      queue ! new SolveQueueTask(metadata.init, line.dropRight(metadata.last._1 + 1), solver)
      return true
    }

    false
  }

  def splitByFirstMaxLength(line: Line, metadata: LineMetadata, solver: LineSolver): Boolean = {
    val stat = countStat(line)
    val indexes = indicesForStat(stat)

    def setClearedAt(i: Int) {
      if (i >= 0 && i < line.size) line(i) = Cleared
    }

    val maxLength = metadata.max

    if (metadata.count(_ == maxLength) != stat.count(_ == (new FilledCell(Color.BLACK), maxLength))) {
      return false
    }

    val i = stat.indices.filter(i => stat(i) == (new FilledCell(Color.BLACK), maxLength)).head

    setClearedAt(indexes(i) - 1)
    setClearedAt(indexes(i+1))

    val m = metadata.splitByFirstChunk(maxLength)

    val metadata1 = m._1
    val line1 = line.dropRight(line.size - indexes(i))

    val metadata2 = m._2.drop(1)
    val line2 = line.drop(indexes(i + 1))

    //println("splitted to: %s,%s, lines: %s, %s".format(metadata1.mkString("[", ",", "]"), metadata2.mkString("[", ",", "]"), line1, line2))
    queue ! new SolveQueueTask(metadata1, line1, solver)
    queue ! new SolveQueueTask(metadata2, line2, solver)

    true
  }

  /**
   * Деление на подсроки. Вызов метода подразумевает, что линия точно поделится на число линий равное metadata.size
   * @param line
   * @param stat
   * @return
   */
  def divideToSublists(line: Line, stat: List[(Cell, Int)]): List[Line] = {
    val statIndicies = indicesForStat(stat)

    /**
     *
     * @param i Индекс текущего найденого кусочка
     * @return true, если после укзанной позиции есть еще заполненные участки.
     *         (Используется для определения необходимости деления)
     */
    def isNotLastFilled(i: Int) = stat.drop(i+1).exists(_._1.isFilled)
    def isFilledBefore(i: Int) = stat.take(i).exists(_._1.isFilled)

    (stat.indices.tail).find(i => { stat(i)._1 == Cleared && isFilledBefore(i) && isNotLastFilled(i)}) match {
      case Some(x) => {
        val splitIndex = statIndicies(x)
        line.dropRight(line.size - splitIndex) +: divideToSublists(line.drop(splitIndex), stat.drop(x))
      }
      case None => List(line)
    }
  }

}
