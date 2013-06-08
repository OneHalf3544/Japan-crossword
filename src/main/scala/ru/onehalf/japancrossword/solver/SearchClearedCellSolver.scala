package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{LineMetadata, Line}
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.line.{Line, LineImpl}

/**
 * Поиск пустых ячеек
 * <p/>
 * <p/>
 * Created: 17.05.13 0:25
 * <p/>
 * @author OneHalf
 */
object SearchClearedCellSolver extends LineSolver {

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   *
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  override def fillLine(currentData: Line): Line = {
    val stat: List[(Cell, Int)] = currentData.countStat()

    val indexes = indicesForStat(stat)
    var preResult = currentData.toList

    if (stat.filter(_._1 == FILLED).corresponds(currentData.metadata.toList)((a, b) => a._2 == b)) {
      // Все уже решено
      return new LineImpl(currentData.metadata, preResult.map(v => if (v == NOT_KNOWN) CLEARED else v))
    }

    for (i <- stat.indices) {

      def prevIsCleared: Boolean = !stat.isDefinedAt(i - 1) || stat(i - 1)._1 == CLEARED
      def nextIsCleared: Boolean = !stat.isDefinedAt(i + 1) || stat(i + 1)._1 == CLEARED
      def isNotKnown: Boolean = stat(i)._1 == NOT_KNOWN && stat(i)._2 < currentData.metadata.min

      if (prevIsCleared && isNotKnown && nextIsCleared) {
        val length = stat(i)._2
        preResult = preResult.patch(indexes(i), List.fill(length)(CLEARED), length)
      }
    }

    for (i <- stat.indices) {
      if (stat(i)._1 == FILLED && stat(i)._2 == currentData.metadata.max) {
        if (indexes.isDefinedAt(i - 1))
          preResult = preResult.updated(indexes(i) - 1, CLEARED)
        if (indexes.isDefinedAt(i + 2))
          preResult = preResult.updated(indexes(i + 1), CLEARED)
      }
    }

    if (currentData.metadata.size == 1) {
      val filledIndexes = preResult.indices.filter(preResult(_) == FILLED)
      if (filledIndexes.nonEmpty) {
        val minIndex = filledIndexes.max - currentData.metadata(0)
        val maxIndex = filledIndexes.min + currentData.metadata(0)

        preResult =
          List.fill[Cell](minIndex + 1)(CLEARED) :::
          preResult.slice(minIndex + 1, maxIndex) :::
          List.fill[Cell](preResult.size - maxIndex)(CLEARED)
      }
    }
    new LineImpl(currentData.metadata, preResult)
  }
}
