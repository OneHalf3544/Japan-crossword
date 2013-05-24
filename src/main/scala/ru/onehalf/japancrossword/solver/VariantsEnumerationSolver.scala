package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.model.Cell._

/**
 * Логика решения кроссворда перебором содержимого строк и столбцов.
 * Все линии перебираются независимо (т.е. зависимось одной строки от соседней не прсчитывается)
 * Тем не менее, строки разной направленности (ряд/столбец) считаются параллельно, используюя одни и те же данные.
 * В результате определение состояния ячейки в ряду может упростить перебор соответствующего столбца
 * <p/>
 * <p/>
 * Created: 07.05.13 7:28
 * <p/>
 * @author OneHalf
 */
object VariantsEnumerationSolver extends LineSolver {

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   */
  def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = {
    fitRemainder(metadata, currentData).get
  }

  /**
   * Заполнение линии согласно переданным в функцию метаданным.
   * Функция рекурсивно вызывает саму себя. Выставляет первый элемент,
   * и для заполнения остатка линии вы зывает этот же метод
   * @param metadata Метаданные для линии
   * @param currentData текущее содержимое линии
   * @return Список линий, подходящих под указанные метаданные
   */
  def fitRemainder(metadata: Array[Int], currentData: LineTrait): Option[List[Cell]] = {

    if (metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      if (currentData.forall(_ != FILLED)) return Option(List.fill[Cell](currentData.size)(CLEARED))
      else return Option.empty // В случае противоречий говорим, что решения нет
    }

    val chunkLength = metadata.head
    val chunk = List.fill[Cell](chunkLength)(FILLED)
    val expectedLength = currentData.size
    val separator = List(CLEARED)

    if (expectedLength == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return Option(chunk)
    }

    var result: Option[List[Cell]] = Option.empty

    for (offset <- 0 to expectedLength - chunkLength - metadata.tail.map(_ + 1).sum) {

      // Заполнение отступом + заполненный участок
      var lineStart: List[Cell] = List.fill[Cell](offset)(CLEARED) ::: chunk

      // Параметр для повтороного вызова метода
      var newCurrentData = currentData.drop(lineStart.size)

      // Если какая-то часть строки еще остается, добавляем разделительную ячейку
      if (newCurrentData.nonEmpty()) {
        lineStart = lineStart ::: separator
        newCurrentData = newCurrentData.drop(1)
      }

      // Сразу проверяем на противоречие модели
      // todo проверять почаще, при подборе оставшейся части строки. (Т.к. другие потоки могут уточнить содержимое)
      if (compatibleToCurrentData(currentData.toList.take(lineStart.size), lineStart)) {

        // Доподбираем оставшуюся часть строки
        val subResult = fitRemainder(metadata.tail, newCurrentData)
        if (subResult.isDefined && compatibleToCurrentData(newCurrentData, subResult.get)) {
          if (result.isEmpty)
            result = Option(lineStart ::: subResult.get)
          else
            result = Option(reduceLines(result.get, (lineStart ::: subResult.get)))
        }
      }
    }

    result
  }
}
