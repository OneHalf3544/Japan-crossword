package ru.onehalf.japancrossword

import model.{Cell, JapanCrosswordModel}
import solver.Orientation

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

    val columnVariants: Array[LineVariants] = (0 to model.columnNumber-1).par.map(fillColumn(_)).toArray
    val rowVariants: Array[LineVariants] = (0 to model.rowNumber-1).par.map(fillRow(_)).toArray

    /**
     * Один цикл подбора вариантов
     */
    def oneSolveCycle() {
      (0 to model.columnNumber-1).par.foreach(columnVariants(_).addDataToModel())
      (0 to model.rowNumber-1).par.foreach(rowVariants(_).addDataToModel())
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
    fillLine(x, Orientation.VERTICAL, model.rowNumber, model.horizonLine(x))
  }

  /**
   * Заполнить строку
   * @param y Номер строки (с нуля)
   */
  def fillRow(y: Int): LineVariants = {
    fillLine(y, Orientation.HORIZONTAL, model.columnNumber, model.verticalLine(y))
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param orientation Тип расположения линии
   * @param lineLength Размер линии
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   */
  def fillLine(lineIndex: Int, orientation : Orientation.Orientation, lineLength: Int,
               metadata: Array[Int]): LineVariants = {
    // Все возможные способы заполнения: линии
    new LineVariants(lineIndex, orientation, fitRemainder(lineLength, metadata), model)
  }

  /**
   * Заполнение линии согласно переданным в функцию метаданным.
   * Функция рекурсивно вызывает саму себя. Выставляет первый элемент,
   * и для заполнения остатка линии вы зывает этот же метод
   * @param remainderCellCount Число ячеек, которые нужно заполнить
   * @param remainder Метаданные для линии
   * @return Список линий, подходящих под указанные метаданные
   */
  def fitRemainder(remainderCellCount: Int, remainder: Array[Int]): Array[List[Cell.Cell]] = {

    if (remainder.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      return Array(List.fill[Cell.Cell](remainderCellCount)(Cell.CLEARED))
    }

    var lines: Array[List[Cell.Cell]] = Array.empty[List[Cell.Cell]]
    for (i <- 0 to remainderCellCount - 1) {

      val newRemainderCount = remainderCellCount - i - remainder(0)

      if (newRemainderCount >= 0) {
        var lineStart: List[Cell.Cell] =
          List.fill[Cell.Cell](i)(Cell.CLEARED) ::: List.fill[Cell.Cell](remainder(0))(Cell.FILLED)

        if (newRemainderCount > 0) {
          lineStart = lineStart ::: List.fill[Cell.Cell](1)(Cell.CLEARED)
        }

        fitRemainder(newRemainderCount - 1, remainder.tail).foreach(p => {lines = lines :+ (lineStart ::: p)})
      }
    }

    lines
  }
}
