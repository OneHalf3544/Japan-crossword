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
class Solver(model: JapanCrosswordModel) {

  /**
   * Запуск решения кроссворда
   */
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

    def addDataToModel(variant: List[Cell.Cell], line: Line) {
      0 to variant.size-1 filter (line(_) == Cell.NOT_KNOWN) foreach(i => line(i) = variant(i))
    }

    (0 to number - 1).toList.sortBy(metadata(_).sum).reverse.par.foreach(v => {
      val line = new Line(v, orientation, model)
      addDataToModel(fillLine(v, line), line)
    })
  }

  /**
   * Заполнить столбец
   * @param x Номер столбца (с нуля)
   */
  def fillColumn(x: Int, currentData: Line) = {
    fillLine(model.horizonLine(x), currentData)
  }

  /**
   * Заполнить строку
   * @param y Номер строки (с нуля)
   */
  def fillRow(y: Int, currentData: Line) = {
    fillLine(model.verticalLine(y), currentData)
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

  /**
   * Схлопывание строки в одну. Если значение в списках не совпадают,
   * то в результате по соответсвующему индексу будет Cell.NOT_KNOWN.
   * В случае совпаднения значений, содержимое попадет в результирующий список
   * @param line1 Строка 1
   * @param line2 Строка 2
   * @return Результат объединения
   */
  private def reduceLines(line1: List[Cell.Cell], line2: List[Cell.Cell]): List[Cell.Cell] = {
    assert(line1.size == line2.size, "lines: " + line1.size + ", " + line2.size)
    val lineLength = line1.size

    val result = Array.fill[Cell.Cell](lineLength)(Cell.NOT_KNOWN)

    // Сохряняем в результат только совпадающие данные
    0 to lineLength -1 filter ((i) => line1(i) == line2(i)) foreach(i => result(i) = line2(i))
    result.toList
  }

  /**
   * Проверка, не противоречит ли предлагаемое значение текущим данным
   * @param currentData Текущие данные в модели
   * @param supposeLine Предлагаемая линия (Может содержать NOT_KNOWN)
   * @return true, если вариант приемлим
   */
  def compatibleToCurrentData(currentData: Line, supposeLine: List[Cell.Cell]): Boolean = {
    compatibleToCurrentData(currentData.toList, supposeLine)
  }

  /**
   * Проверка, не противоречит ли предлагаемое значение текущим данным
   * @param currentData Текущие данные в модели
   * @param supposeLine Предлагаемая линия (Может содержать NOT_KNOWN)
   * @return true, если вариант приемлим
   */
  def compatibleToCurrentData(currentData: List[Cell.Cell], supposeLine: List[Cell.Cell]): Boolean = {

    def cellIsCompatible(i: Int): Boolean = {
      currentData(i) == supposeLine(i) || currentData(i) == Cell.NOT_KNOWN || supposeLine(i) == Cell.NOT_KNOWN
    }

    assert(currentData.size == supposeLine.size)
    0 to supposeLine.size-1 forall (cellIsCompatible(_))
  }
}
