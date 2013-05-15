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
        Thread.sleep(1000)
        // todo сделать нормальное условие выхода
      } while (!model.isSolved)
    }
    println("border solve end")
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  // todo Проверять соответствие модели, для обнаружения некорректных кросвордов
  def fillLine(metadata: Array[Int], currentData: LineTrait): List[Cell] = {
    val begunNotClearedIndex = (0 to currentData.size-1).filterNot(currentData(_) == CLEARED).headOption
    if (begunNotClearedIndex.isEmpty) {
      // Строка уже решена
      return currentData.toList
    }

    val nextCleared = (1 to metadata(0))
      .map(_ + begunNotClearedIndex.get - 1)
      .filter(currentData(_) == CLEARED)
      .headOption

    if (nextCleared.isDefined) {
      (begunNotClearedIndex.get to nextCleared.get - 1) foreach (currentData(_) = CLEARED)
      return currentData.toList
    }

    val filledAfter = nextFilledCount(currentData.toList.drop(begunNotClearedIndex.get + metadata(0)))
    if (filledAfter > 0) {
      (1 to filledAfter) map(_ + begunNotClearedIndex.get - 1) foreach (currentData(_) = CLEARED)
      return currentData.toList
    }

    val chunkStartIndex = (1 to metadata(0))
      .map(_ + begunNotClearedIndex.get - 1)
      .filter(currentData(_) == FILLED)
      .headOption

    if (chunkStartIndex.isEmpty) {
      // Нет возможностей для расчета
      return currentData.toList
    }

    // Заполняем кусочек линии, который перекрывается при любом варианте
    if (begunNotClearedIndex.get == chunkStartIndex.get) {
      // Здесь мы сразу знаем кусочек целиком + завершающий индекс
      (1 to metadata(0)).map(_ + begunNotClearedIndex.get - 1).foreach(currentData(_) = FILLED)
      currentData(metadata(0) + begunNotClearedIndex.get) = CLEARED
      // todo Пускать решение рекурсивно,
      return currentData.toList
    }

    // Заполняем кусочек линии, который перекрывается при любом варианте
    (1 to metadata(0))
      .map(_ + begunNotClearedIndex.get - 1)
      .filter(_ > chunkStartIndex.get)
      .foreach(currentData(_) = FILLED)

    currentData.toList
  }

  def nextFilledCount(list: List[Cell]): Int = {
    if (list.head != FILLED) 0
    else 1 + nextFilledCount(list.tail)
  }
}
