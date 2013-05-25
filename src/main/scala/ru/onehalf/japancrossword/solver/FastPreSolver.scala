package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.LineTrait
import ru.onehalf.japancrossword.model.Cell._

/**
 * <p/>
 * <p/>
 * Created: 12.05.13 22:51
 * <p/>
 * @author OneHalf
 */
object FastPreSolver extends LineSolver {
  // todo Использовать не только при старте решения

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   */
  def fillLine(metadata: Array[Int], currentData: LineTrait): List[Cell] = {

    val length = metadata.sum + metadata.size - 1
    assert(currentData.size >= length, "wrong length: " + length)

    if (metadata.isEmpty) {
      return List.fill(currentData.size)(CLEARED)
    }

    val separator = List(CLEARED)

    // Последовательность, которую будем двигать от края до края
    var cellSequence = List.fill(metadata.head)(FILLED)
    if (metadata.size > 1)
      cellSequence = cellSequence ::: metadata.tail.map(s => separator ::: List.fill(s)(FILLED)).reduce((a, b) => a ::: b)

    var variants: List[List[Cell]] = List()
    for (i <- 0 to currentData.size - length) {
      variants = variants ::: List(List.fill(i)(CLEARED) ::: cellSequence ::: List.fill(currentData.size - i - length)(CLEARED))
    }

    variants.reduce(reduceLines)
  }
}
