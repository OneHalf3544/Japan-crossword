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

  def dropClearedFromEnds(lineTrait: Line)

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return true, если линия разделена
   */
  def splitLine(metadata: Array[Int], currentData: Line, solver: LineSolver): Boolean = {

    if (currentData(0) == CLEARED || currentData.last == CLEARED) {
      dropClearedFromEnds(currentData)
      return true
    }

    if (countStat(currentData.toList.filterNot(_ == NOT_KNOWN)).count(_._1 == FILLED) < metadata.size) {
      return false
    }

    val sublists = divideToSublists(currentData, countStat(currentData))
    assert(sublists.size == metadata.size, "size not equals: %s and %s".format(sublists, metadata.mkString("[", ",", "]")))
    sublists.indices map (v => queue ! new SolveQueueTask(Array(metadata(v)), sublists(v), solver))

    true
  }

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
