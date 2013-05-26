package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.Line
import ru.onehalf.japancrossword.model.Cell._

/**
 * Определение состояния ячеек ближайших к краям.
 * Пример:
 * <p/>
 * Строка:
 * <pre>4 2 5 | ..X...........X.</pre>
 * Превращается в:
 * <pre>4 2 5 | ..XX.......XXXX.</pre>
 * <p/>
 * <p/>
 * Created: 14.05.13 10:28
 * <p/>
 * @author OneHalf
 */
object BorderSolver extends LineSolver {

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param oldCurrentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  def fillLine(metadata: Array[Int], oldCurrentData: Line): List[Cell] = {
    fillLine2(metadata, oldCurrentData)
    oldCurrentData.toList
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  // todo Проверять соответствие модели, для обнаружения некорректных кросвордов
  def fillLine2(metadata: Array[Int], currentData: Line) {

    val firstChunkLength = metadata(0)
    if (firstChunkLength > currentData.size) {
      return
    }

    if ((0 to firstChunkLength - 1) forall(currentData(_) == FILLED) ) {
      if (firstChunkLength + 1 >= currentData.size)
        // Мы решили строку до конца
        return

      currentData(firstChunkLength) = CLEARED
      fillLine(metadata.tail, currentData.drop(firstChunkLength))
      return
    }

    val nextCleared = (0 to firstChunkLength-1).find(currentData(_) == CLEARED)
    if (nextCleared.isDefined) {
      // Сюда не влезет закрашенная часть ожидаемой длины. Помечаем ячейки как пустые
      (0 to nextCleared.get - 1) foreach (currentData(_) = CLEARED)
      return
    }

    if (((firstChunkLength == currentData.size) || currentData(firstChunkLength) == CLEARED) && ((0 to firstChunkLength - 1) exists (currentData(_) == FILLED))) {
      // Тут мы нашли кусочек по ограничению с правой стороны. Закрашиваем найденное
      (0 to firstChunkLength-1) foreach (currentData(_) = FILLED)
      return
    }

    // Число закрашенных ячеек после ожидаемого (позволяет закрасить столько же ячеек от начала строки)
    val filledAfter = nextFilledCount(currentData.toList.drop(firstChunkLength))
    if (filledAfter > 0) {
      (0 to filledAfter-1) foreach (currentData(_) = CLEARED)
      return
    }

    // Находим первую закрашеную ячейку. После нее можем закрасить несколько следующих ячеек,
    // в зависимости от ожидаемого размера закрашенной линии
    val chunkStartIndex = (0 to firstChunkLength-1).find(currentData(_) == FILLED)
    if (chunkStartIndex.isEmpty) {
      // Нет возможностей для расчета
      return
    }

    if (chunkStartIndex.get == 0) {
      // Здесь мы сразу знаем кусочек целиком + завершающий индекс
      (0 to firstChunkLength-1)
        .filter(_ < currentData.size)
        .foreach(currentData(_) = FILLED)

      if(firstChunkLength < currentData.size) {
        currentData(firstChunkLength) = CLEARED
      }
      return
    }

    // Заполняем кусочек линии, который перекрывается при любом варианте
    (0 to firstChunkLength-1)
      .filter(_ > chunkStartIndex.get)
      .foreach(currentData(_) = FILLED)
  }

  def nextFilledCount(list: List[Cell]): Int = {
    if (list.isEmpty || list.head != FILLED) 0
    else 1 + nextFilledCount(list.tail)
  }
}
