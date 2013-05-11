package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel}

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
      // todo Запускать строки/стробцы паралельно, в порядке от простых вариантов к сложным
      // todo (чтобы при подборе вариантов ячейки накладывались уточнялили перебор соседним потокам другими линиями)
      (0 to model.columnNumber-1).toList.sortBy(model.horizonLine(_) .sum).reverse.take(4).par.map(fillColumn(_)).foreach(_.addDataToModel())
      (0 to model.rowNumber-1)   .toList.sortBy(model.verticalLine(_).sum).reverse.take(4).par.map(fillRow(_))   .foreach(_.addDataToModel())
    }

    var oldUnresolvedCount = model.totalUnresolvedCount() + 1

    oneSolveCycle()

    // Продолжаем подбирать варианты, пока решение не зайдет в тупик, либо не завершится успехом
    while (!model.isSolved && oldUnresolvedCount != model.totalUnresolvedCount) {
      oldUnresolvedCount = model.totalUnresolvedCount()
      oneSolveCycle()
    }
  }

  /**
   * Заполнить столбец
   * @param x Номер столбца (с нуля)
   */
  def fillColumn(x: Int): LineVariants = {
    val currentData = (0 to model.rowNumber - 1) map (model(x, _))
    fillLine(x, Orientation.VERTICAL, model.horizonLine(x), currentData.toList)
  }

  /**
   * Заполнить строку
   * @param y Номер строки (с нуля)
   */
  def fillRow(y: Int): LineVariants = {
    val currentData = (0 to model.columnNumber - 1) map (model(_, y))
    fillLine(y, Orientation.HORIZONTAL, model.verticalLine(y), currentData.toList)
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param orientation Тип расположения линии
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   */
  def fillLine(lineIndex: Int, orientation : Orientation.Orientation,
               metadata: Array[Int], currentData: List[Cell.Cell]): LineVariants = {
    // Все возможные способы заполнения: линии
    new LineVariants(lineIndex, orientation, Array(fitRemainder(metadata, currentData).get), model)
  }

  /**
   * Заполнение линии согласно переданным в функцию метаданным.
   * Функция рекурсивно вызывает саму себя. Выставляет первый элемент,
   * и для заполнения остатка линии вы зывает этот же метод
   * @param metadata Метаданные для линии
   * @param currentData текущее содержимое линии
   * @return Список линий, подходящих под указанные метаданные
   */
  def fitRemainder(metadata: Array[Int], currentData: List[Cell.Cell]): Option[List[Cell.Cell]] = {

    if (metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      if (currentData.forall(_ != Cell.FILLED)) return Option(List.fill[Cell.Cell](currentData.size)(Cell.CLEARED))
      else return Option.empty // В случае противоречий говорим, что решения нет
    }

    val chunkLength = metadata.head
    val expectedLength = currentData.size

    var result: Option[List[Cell.Cell]] = Option.empty

    (0 to currentData.size - 1).filter(_ + chunkLength <= expectedLength).foreach(i => {

      // Заполнение отступом + заполненный участок
      var lineStart: List[Cell.Cell] =
        List.fill[Cell.Cell](i)(Cell.CLEARED) ::: List.fill[Cell.Cell](chunkLength)(Cell.FILLED)

      // Параметр для повтороного вызова метода
      var newCurrentData = currentData.drop(lineStart.size)

      // Если какая-то часть строки еще остается, добавляем разделительную ячейку
      if (newCurrentData.nonEmpty) {
        lineStart = lineStart ::: List(Cell.CLEARED)
        newCurrentData = newCurrentData.drop(1)
      }

      // Доподбираем оставшуюся часть строки
      val subResult = fitRemainder(metadata.tail, newCurrentData)
      if (subResult.isDefined)
        // Сохраняем найденный вариант в акумулятор
        if (compatibleToCurrentData(newCurrentData, subResult.get)) {
          if (result.isEmpty)
            result = Option(lineStart ::: subResult.get)
          else
            result = Option(reduceLines(result.get, (lineStart ::: subResult.get)))
        }
    })

    result
  }

  private def reduceLines(line1: List[Cell.Cell], line2: List[Cell.Cell]): List[Cell.Cell] = {
    assert(line1.size == line2.size, "lines: " + line1.size + ", " + line2.size)
    val lineLength = line1.size

    val result = Array.fill[Cell.Cell](lineLength)(Cell.NOT_KNOWN)

    0 to lineLength -1 filter ((i) => line1(i) == line2(i)) foreach(i => result(i) = line1(i))
    result.toList
  }

  /**
   * Проверка, не противоречит ли предлагаемое значение текущим данным
   * @param supposeLine Предлагаемая линия
   * @return true, если вариант приемлим
   */
  def compatibleToCurrentData(currentData: List[Cell.Cell], supposeLine: List[Cell.Cell]): Boolean = {
    assert(currentData.size == supposeLine.size)

    0 to supposeLine.size-1 forall (i => currentData(i) == Cell.NOT_KNOWN || currentData(i) == supposeLine(i) || supposeLine(i) == Cell.NOT_KNOWN)
  }
}
