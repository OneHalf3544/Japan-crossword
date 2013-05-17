package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{LineTrait, JapanCrosswordModel, Line}
import ru.onehalf.japancrossword.model.Cell._
import concurrent._
import ExecutionContext.Implicits.global

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
class BorderSolver(model: JapanCrosswordModel) extends Solver(model) {
  /**
   * Запуск решения кроссворда
   */
  def solve() {
    println("border solve start")

    future {
      do {
        (0 to model.rowNumber - 1).foreach(v => {
          val line = new Line(v, Orientation.HORIZONTAL, model)
          fillRow(v, line)
          fillLine(model.verticalLine(v).reverse, line.reverse())
        })
        (0 to model.columnNumber - 1).foreach(v => {
          val line = new Line(v, Orientation.VERTICAL, model)
          fillColumn(v, line)
          fillLine(model.horizonLine(v).reverse, line.reverse())
        })
        Thread.sleep(100)
        // todo сделать нормальное условие выхода
      } while (!model.isSolved)
      println("border solve end")
    }.onFailure{
      case e: Exception => println(e.printStackTrace())
    }
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param oldCurrentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  // todo Проверять соответствие модели, для обнаружения некорректных кросвордов
  def fillLine(metadata: Array[Int], oldCurrentData: LineTrait): List[Cell] = {
    if (metadata.isEmpty) {
      (0 to oldCurrentData.size - 1).foreach(oldCurrentData(_) = CLEARED)
      oldCurrentData.toList
    }

    val begunNotClearedIndex = (0 to oldCurrentData.size-1).filterNot(oldCurrentData(_) == CLEARED).headOption
    if (begunNotClearedIndex.isEmpty) {
      // Строка уже решена
      return oldCurrentData.toList
    }

    val currentData = oldCurrentData.drop(begunNotClearedIndex.get)
    val firstChunkLength = metadata(0)

    if (firstChunkLength > currentData.size) {
      return oldCurrentData.toList
    }

    if ((0 to firstChunkLength - 1) forall(currentData(_) == FILLED) ) {
      if (firstChunkLength + 1 < currentData.size)
        currentData(firstChunkLength) = CLEARED

      fillLine(metadata.tail, currentData.drop(firstChunkLength))
      return oldCurrentData.toList
    }

    val nextCleared = (0 to firstChunkLength-1)
      .filter(currentData(_) == CLEARED)
      .headOption

    if (nextCleared.isDefined) {
      (0 to nextCleared.get - 1) foreach (currentData(_) = CLEARED)
      return oldCurrentData.toList
    }

    if (currentData(firstChunkLength) == CLEARED && ((0 to firstChunkLength - 1) exists (currentData(_) == FILLED))) {
      (1 to firstChunkLength) map (_ - 1) foreach (currentData(_) = FILLED)
    }

    // Число закрашенных ячеек после ожидаемого (позволяет закрасить столько же ячеек от начала строки)
    val filledAfter = nextFilledCount(currentData.toList.drop(firstChunkLength))
    if (filledAfter > 0) {
      (0 to filledAfter-1) foreach (currentData(_) = CLEARED)
      return oldCurrentData.toList
    }

    val chunkStartIndex = (0 to firstChunkLength-1)
      .filter(currentData(_) == FILLED)
      .headOption

    if (chunkStartIndex.isEmpty) {
      // Нет возможностей для расчета
      return oldCurrentData.toList
    }

    // Заполняем кусочек линии, который перекрывается при любом варианте
    if (chunkStartIndex.get == 0) {
      // Здесь мы сразу знаем кусочек целиком + завершающий индекс
      (0 to firstChunkLength-1)
        .filter(_ < currentData.size)
        .foreach(currentData(_) = FILLED)

      if(firstChunkLength < currentData.size) {
        currentData(firstChunkLength) = CLEARED
      }
      // todo Пускать решение рекурсивно,
      return oldCurrentData.toList
    }

    // Заполняем кусочек линии, который перекрывается при любом варианте
    (0 to firstChunkLength-1)
      .filter(_ > chunkStartIndex.get)
      .foreach(currentData(_) = FILLED)

    oldCurrentData.toList
  }

  def nextFilledCount(list: List[Cell]): Int = {
    if (list.isEmpty || list.head != FILLED) 0
    else 1 + nextFilledCount(list.tail)
  }
}
