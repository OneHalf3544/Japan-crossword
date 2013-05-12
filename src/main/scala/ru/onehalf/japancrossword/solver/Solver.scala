package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{Line, Metadata, Cell, JapanCrosswordModel}
import concurrent._
import duration.Duration
import ExecutionContext.Implicits.global

/**
 * Логика решения кроссворда
 * <p/>
 * <p/>
 * Created: 07.05.13 7:28
 * <p/>
 * @author OneHalf
 */
class Solver(model: JapanCrosswordModel) extends SolverTrait(model) {

  def solve() {

    /**
     * Один цикл подбора вариантов
     */
    def oneSolveCycle() {

      // todo рассинхронизировать циклы по столбцам/строкам.
      // т.е. сейчас когда обход столбцов законцился, а строк еще нет,
      // производится ожидание завершения подбора строк,
      // вместо асинхронного запуска перебора столбцов заново

      println("one solve cycle start")

      val columnFuture = future {
        futureRun(model.columnNumber, model.horizonLine, fillColumn, Orientation.VERTICAL)
      }
      val rowFuture = future {
        futureRun(model.rowNumber, model.verticalLine, fillRow, Orientation.HORIZONTAL)
      }

      Await.ready(columnFuture, Duration.Inf)
      Await.ready(rowFuture,    Duration.Inf)

      println("one solve cycle ended")
    }

    var oldUnresolvedCount = model.totalUnresolvedCount() + 1

    oneSolveCycle()

    // Продолжаем подбирать варианты, пока решение не зайдет в тупик, либо не завершится успехом
    while (!model.isSolved && oldUnresolvedCount != model.totalUnresolvedCount) {
      oldUnresolvedCount = model.totalUnresolvedCount()
      oneSolveCycle()
    }
    println("solve ended")
  }

  def futureRun(number: Int, metadata: Metadata, fillLine: (Int, Line) => List[Cell.Cell],
                orientation: Orientation.Orientation) {

    (0 to number - 1).toList.sortBy(metadata(_).sum).reverse.par.foreach(v => {
      val line = new Line(v, orientation, model)
      addDataToModel(fillLine(v, line), line)
    })
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   */
  def fillLine(metadata: Array[Int], currentData: Line): List[Cell.Cell] = {
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
  def fitRemainder(metadata: Array[Int], currentData: Line): Option[List[Cell.Cell]] = {

    if (metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      if (currentData.forall(_ != Cell.FILLED)) return Option(List.fill[Cell.Cell](currentData.size)(Cell.CLEARED))
      else return Option.empty // В случае противоречий говорим, что решения нет
    }

    val chunkLength = metadata.head
    val chunk = List.fill[Cell.Cell](chunkLength)(Cell.FILLED)
    val expectedLength = currentData.size
    val separator = List(Cell.CLEARED)

    if (expectedLength == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return Option(chunk)
    }

    // todo Выпилить использование Option, либо проверять здесь согласованность данных,
    // для проверки, что текущее содержимое модели не нарушает условий
    var result: Option[List[Cell.Cell]] = Option.empty

    for (offset <- 0 to expectedLength - chunkLength) {

      // Заполнение отступом + заполненный участок
      var lineStart: List[Cell.Cell] = List.fill[Cell.Cell](offset)(Cell.CLEARED) ::: chunk

      // Параметр для повтороного вызова метода
      var newCurrentData = currentData.drop(lineStart.size)

      // Если какая-то часть строки еще остается, добавляем разделительную ячейку
      if (newCurrentData.nonEmpty()) {
        lineStart = lineStart ::: separator
        newCurrentData = newCurrentData.drop(1)
      }

      // Сразу проверяем на противоречие модели
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
