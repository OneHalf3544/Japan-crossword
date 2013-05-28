package ru.onehalf.japancrossword.solver

import queue.{SolveLineQueue, SolveQueueTask}
import ru.onehalf.japancrossword.model.Line
import ru.onehalf.japancrossword.model.Cell._

/**
 * <p/>
 * <p/>
 * Created: 26.05.13 22:45
 * <p/>
 * @author OneHalf
 */
class LineSplitter(queue: SolveLineQueue) extends LineSolver{


  def fillLine(metadata: Array[Int], currentData: Line): List[Cell] = {throw new UnsupportedOperationException}

  def dropClearedFromEnds(line: Line): Line = {
    if (line(0) == CLEARED) {
      return dropClearedFromEnds(line.drop(1))
    }
    if (line.last == CLEARED) {
      return dropClearedFromEnds(line.dropRight(1))
    }
    line
  }

  def dropChanksFromEnds(metadata: Array[Int], line: Line, solver: LineSolver): Boolean = {
    val stat = countStat(line)

    if (stat.head == (FILLED, metadata.head) && stat(1)._1 == CLEARED) {
      queue ! new SolveQueueTask(metadata.tail, line.drop(metadata.head+1), solver)
      return true
    }

    if (stat.last == (FILLED, metadata.last) && stat(stat.size-2)._1 == CLEARED) {
      queue ! new SolveQueueTask(metadata.init, line.dropRight(metadata.last+1), solver)
      return true
    }

    false
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return true, если линия разделена
   */
  def splitLine(metadata: Array[Int], currentData: Line, solver: LineSolver): Boolean = {

    if (currentData(0) == CLEARED || currentData.last == CLEARED) {
      queue ! new SolveQueueTask(metadata, dropClearedFromEnds(currentData), solver)
      return true
    }

    if (dropChanksFromEnds(metadata, currentData, solver)) {
      return true
    }

    if (metadata.size == 1) {
      return false
    }

    if (countStat(currentData.toList.filterNot(_ == NOT_KNOWN)).count(_._1 == FILLED) >= metadata.size) {
      val sublists = divideToSublists(currentData, countStat(currentData))
      assert(sublists.size == metadata.size, "size not equals: %s and %s".format(metadata.mkString("[", ",", "]"), sublists))
      sublists.indices map (v => queue ! new SolveQueueTask(Array(metadata(v)), sublists(v), solver))
      return true
    }

    splitByFirstMaxLength(currentData, metadata, solver)
  }


  def splitByFirstMaxLength(line: Line, metadata: Array[Int], solver: LineSolver): Boolean = {
    val stat = countStat(line)
    val indexes = indicesForStat(stat)

    def isClearedAt(i: Int): Boolean = {
      (!stat.isDefinedAt(i) || stat(i)._1 == CLEARED)
    }

    val maxLength = metadata.max

    if (metadata.count(_ == maxLength) != stat.count(_ == (FILLED, maxLength))) {
      return false
    }

    stat.indices
      .find(i => (stat(i) == (FILLED, maxLength) && isClearedAt(i - 1) && isClearedAt(i + 1))) match {
      case Some(i) => {
        val m = metadata.splitAt(metadata.find(_ == maxLength).get)

        queue ! new SolveQueueTask(m._1.dropRight(1), line.dropRight(line.size - indexes(i)), solver)
        queue ! new SolveQueueTask(m._2, line.drop(indexes(i+1)), solver)

        true
      }
      case None => false
    }
  }

  /**
   * Деление на подсроки. Вызов метода подразумевает, что линия точно поделится на число линий равное metadata.size
   * @param line
   * @param stat
   * @return
   */
  def divideToSublists(line: Line, stat: List[(Cell, Int)]): List[Line] = {
    val statIndicies = indicesForStat(stat)
    var hasFilledCell = false

    for (i <- stat.indices) {
      if (stat(i)._1 == CLEARED && hasFilledCell && (stat.drop(i).exists(_._1 == FILLED))) {
        val splitIndex = statIndicies(i)
        return line.dropRight(line.size - splitIndex) +: divideToSublists(line.drop(splitIndex), stat.drop(i))
      }
      if (stat(i)._1 == FILLED) {
        hasFilledCell = true
      }
    }
    List(line)
  }

}
