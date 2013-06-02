package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.Line
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.Cell.Cell

/**
 * <p/>
 * <p/>
 * Created: 12.05.13 22:51
 * <p/>
 * @author OneHalf
 */
object FastPreSolver extends LineSolver {

  val SEPARATOR = List(CLEARED)

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   */
  def fillLine(metadata: Array[Int], currentData: Line): List[Cell] = {
    assert(metadata.nonEmpty)

    val length = metadata.sum + metadata.size - 1
    assert(currentData.size >= length, "wrong length: " + length)

    val cellSequence: List[Cell] = createSequence(metadata)

    (0 to currentData.size - length)
      .map(i => List.fill(i)(CLEARED) ::: cellSequence ::: List.fill(currentData.size - i - length)(CLEARED))
      .reduce(reduceLines)
  }

  /**
   * @return Последовательность, которую будем двигать от края до края
   */
  def createSequence(metadata: Array[Int]): List[Cell] = {
    val cellSequence = List.fill(metadata.head)(FILLED)

    if (metadata.size <= 1) {
      return cellSequence
    }

    cellSequence ::: metadata.tail.map(s => SEPARATOR ::: List.fill(s)(FILLED)).reduce((a, b) => a ::: b)
  }
}
