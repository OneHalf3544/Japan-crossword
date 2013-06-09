package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.model.Cell
import java.awt.Color

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
  override def fillLine(metadata: LineMetadata, currentData: Line): List[Cell] = {
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
  private[solver] def fitRemainder(metadata: LineMetadata, currentData: Line): Option[List[Cell]] = {

    if (metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      if (currentData.exists(_.isFilled))
        return Option.empty // В случае противоречий говорим, что решения нет
      else
        return Option(List.fill[Cell](currentData.size)(Cleared))
    }

    val chunkLength = metadata.head
    val chunk = List.fill[Cell](chunkLength)(new FilledCell(Color.BLACK))
    val expectedLength = currentData.size
    val separator = List(Cleared)

    if (expectedLength == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return Option(chunk)
    }

    // Не пересчитывать заведомо неопределяемые ячейки
    val maxSequenceLength = metadata.sum + metadata.length - 1
    if (currentData.forall(_.isNotKnown) && currentData.size >= 2 * maxSequenceLength ) {
      return Option(currentData.toList)
    }

    var result: Option[List[Cell]] = Option.empty

    for (offset <- 0 to expectedLength - chunkLength - metadata.tail.map(_ + 1).sum) {

      // Заполнение отступом + заполненный участок
      var lineStart: List[Cell] = List.fill[Cell](offset)(Cleared) ::: chunk

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
          result = Option(if (result.isEmpty)
            lineStart ::: subResult.get
          else
            reduceLines(result.get, lineStart ::: subResult.get))
        }
      }
    }

    result
  }
}
