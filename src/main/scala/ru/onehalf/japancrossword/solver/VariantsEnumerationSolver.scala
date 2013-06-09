package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.line.LineMetadata.metadata
import ru.onehalf.japancrossword.model.line.{Line, LineImpl, LineMetadata}

import scala.collection.mutable

/**
  * Логика решения кроссворда перебором содержимого строк и столбцов.
  * Все линии перебираются независимо (т.е. зависимось одной строки от соседней не прсчитывается)
  * Тем не менее, строки разной направленности (ряд/столбец) считаются параллельно, используюя одни и те же данные.
  * В результате определение состояния ячейки в ряду может упростить перебор соответствующего столбца
  *
  * @since 07.05.13 7:28
  * @author OneHalf
  */
object VariantsEnumerationSolver extends LineSolver {

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
    *
   * @param currentData Текущие данные
   */
  override def fillLine(currentData: Line): Line =
    getResultFromCache(currentData, mutable.HashMap.empty).get

  /**
    * Заполнение линии согласно переданным в функцию метаданным.
    * Функция рекурсивно вызывает саму себя. Выставляет первый элемент,
    * и для заполнения остатка линии вы зывает этот же метод
    *
    * @param currentData текущее содержимое линии
    * @return Список линий, подходящих под указанные метаданные
    */
  private[solver] def getResultFromCache(currentData: Line, cache: mutable.HashMap[Line, Option[Line]]): Option[Line] = {
    if (currentData.metadata.isEmpty) {
      // Is there no more metadata? It seems the rest of line is empty
      if (currentData.forall(_ != FILLED))
        return Option(new LineImpl(LineMetadata.empty(), Array.fill(currentData.size)(CLEARED)))
      else
        return None // В случае противоречий говорим, что решения нет
    }

    val cachedResult: Option[Option[Line]] = cache.get(currentData)
    if (cachedResult.isDefined) {
      return cachedResult.get
    }

    val result = calculateRemainderWithCache(currentData, cache)
    cache.put(currentData, result)
    result
  }

  private[solver] def calculateRemainderWithCache(currentData: Line,
                                                  cache: mutable.HashMap[Line, Option[Line]]): Option[Line] = {
    assert(currentData.metadata.nonEmpty)

    val chunkLength = currentData.metadata.head
    val chunk = Array.fill[Cell](chunkLength)(FILLED)

    if (currentData.size == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return Option(new LineImpl(metadata(chunkLength), chunk))
    }

    // Не пересчитывать заведомо неопределяемые ячейки
    val maxSequenceLength = currentData.metadata.minimalLineLength
    if (currentData.forall(_.isNotKnown) && currentData.size >= 2 * maxSequenceLength ) {
      return Option(currentData)
    }

    var result: Option[Line] = Option.empty

    for (offset <- 0 to currentData.size - maxSequenceLength + 1) {

      var lineStart: Line = createNewLine(currentData, offset, chunk)

      // Сразу проверяем на противоречие модели
      if (currentData.canStartsWith(lineStart)) {
        // Доподбираем оставшуюся часть строки
        val newCurrentData: Line = currentData.dropFromBegining(lineStart)
        val subResult: Option[Line] = getResultFromCache(newCurrentData, cache)
        if (subResult.isDefined && compatibleToCurrentData(newCurrentData, subResult.get)) {
          result = Option(if (result.isEmpty)
            lineStart ++ subResult.get
          else
            reduceLines(result.get, lineStart ++ subResult.get))
        }
      }
    }
    result
  }

  private def createNewLine(currentData: Line, offset: Int, chunk: Array[Cell]): Line = {
    // Заполнение отступом + заполненный участок
    var lineStart: Array[Cell] = Array.fill[Cell](offset)(CLEARED) ++ chunk

    // Параметр для повтороного вызова метода

    // Если какая-то часть строки еще остается, добавляем разделительную ячейку
    if (currentData.metadata.size > 1) {
      lineStart = lineStart :+ CLEARED
    }
    new LineImpl(metadata(chunk.length), lineStart)
  }

}
