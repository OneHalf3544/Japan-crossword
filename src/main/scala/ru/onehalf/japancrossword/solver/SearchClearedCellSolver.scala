package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.model.Line

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
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  def fillLine(metadata: LineMetadata, currentData: Line): List[Cell] = {
    val stat: List[(Cell, Int)] = countStat(currentData)

    val indexes = indicesForStat(stat)
    var preResult = currentData.toList

    if (stat.filter(_._1.isFilled).corresponds(metadata.toList)((a, b) => a._2 == b._1)) {
      // Все уже решено
      return preResult.map(v => if (v.isNotKnown) Cleared else v)
    }

    for (i <- stat.indices) {

      def prevIsCleared: Boolean = !stat.isDefinedAt(i - 1) || stat(i - 1)._1 == Cleared
      def nextIsCleared: Boolean = !stat.isDefinedAt(i + 1) || stat(i + 1)._1 == Cleared
      def isNotKnown: Boolean = stat(i)._1.isNotKnown && stat(i)._2 < metadata.min

      if (prevIsCleared && isNotKnown && nextIsCleared) {
        val length = stat(i)._2
        preResult = preResult.patch(indexes(i), List.fill(length)(Cleared), length)
      }
    }

    for (i <- stat.indices) {
      if (stat(i)._1.isFilled && stat(i)._2 == metadata.max) {
        if (indexes.isDefinedAt(i - 1))
          preResult = preResult.updated(indexes(i) - 1, Cleared)
        if (indexes.isDefinedAt(i + 2))
          preResult = preResult.updated(indexes(i + 1), Cleared)
      }
    }

    if (metadata.size == 1) {
      val filledIndexes = preResult.indices.filter(preResult(_).isFilled)
      if (filledIndexes.nonEmpty) {
        val minIndex = filledIndexes.max - metadata(0)._1
        val maxIndex = filledIndexes.min + metadata(0)._1

        preResult =
          List.fill[Cell](minIndex + 1)(Cleared) :::
          preResult.slice(minIndex + 1, maxIndex) :::
          List.fill[Cell](preResult.size - maxIndex)(Cleared)
      }
    }
    preResult
  }
}
