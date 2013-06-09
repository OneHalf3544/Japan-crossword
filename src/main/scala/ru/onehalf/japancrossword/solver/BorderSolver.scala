package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{LineMetadata, Line}
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.line.Line

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
   *
   * @param oldCurrentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  override def fillLine(oldCurrentData: Line): Line = {
    fillLine2(oldCurrentData)
    oldCurrentData
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   *
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  // todo Проверять соответствие модели, для обнаружения некорректных кросвордов
  def fillLine2(currentData: Line) {

    val firstChunkLength = currentData.metadata(0)
    if (firstChunkLength > currentData.size) {
      // todo кидать исключение, или заполнить всю currentData
      return
    }

    if (0 until firstChunkLength forall(currentData(_) == new FilledCell(Color.BLACK)) ) {
      if (firstChunkLength + 1 >= currentData.size)
        // Мы решили строку до конца
        return

      currentData(firstChunkLength) = Cleared
      return
    }

    val nextCleared = (0 until firstChunkLength).find(currentData(_) == Cleared)
    if (nextCleared.isDefined) {
      // Сюда не влезет закрашенная часть ожидаемой длины. Помечаем ячейки как пустые
      assert(0 until nextCleared.get forall(currentData(_) != new FilledCell(Color.BLACK)))
      0 until nextCleared.get foreach (currentData(_) = Cleared)
      return
    }

    if (((firstChunkLength == currentData.size) || currentData(firstChunkLength) == Cleared)
      && (0 until firstChunkLength exists (currentData(_).isFilled))) {
      // Тут мы нашли кусочек по ограничению с правой стороны. Закрашиваем найденное
      0 until firstChunkLength foreach (currentData(_) = new FilledCell(Color.BLACK))
      return
    }

    // Число закрашенных ячеек после ожидаемого (позволяет закрасить столько же ячеек от начала строки)
    val filledAfter = nextFilledCount(currentData.toList.drop(firstChunkLength))
    if (filledAfter > 0) {
      0 until filledAfter foreach (currentData(_) = Cleared)
      return
    }

    // Находим первую закрашеную ячейку. После нее можем закрасить несколько следующих ячеек,
    // в зависимости от ожидаемого размера закрашенной линии
    val chunkStartIndex = (0 until firstChunkLength).find(currentData(_).isFilled)
    if (chunkStartIndex.isEmpty) {
      // Нет возможностей для расчета
      return
    }

    if (chunkStartIndex.get == 0) {
      // Здесь мы сразу знаем кусочек целиком + завершающий индекс
      (0 until firstChunkLength)
        .filter(_ < currentData.size)
        .foreach(currentData(_) = new FilledCell(Color.BLACK))

      if(firstChunkLength < currentData.size) {
        currentData(firstChunkLength) = Cleared
      }
      return
    }

    // Заполняем кусочек линии, который перекрывается при любом варианте
    (0 until firstChunkLength)
      .filter(_ > chunkStartIndex.get)
      .foreach(currentData(_) = new FilledCell(Color.BLACK))
  }

  def nextFilledCount(list: List[Cell]): Int = {
    if (list.isEmpty || !list.head.isFilled) 0
    else 1 + nextFilledCount(list.tail)
  }
}
