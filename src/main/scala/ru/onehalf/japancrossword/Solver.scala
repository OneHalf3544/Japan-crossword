package ru.onehalf.japancrossword

import model.{Cell, JapanCrosswordModel}

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

    var oldUnresolvedCount = model.totalUnresolvedCount() + 1

    1 to model.columnNumber foreach (x => fillColumn(x - 1))
    1 to model.rowNumber foreach (y => fillRows(y - 1))

    // Продолжаем подбирать варианты, пока решение не зайдет в тупик, либо не завершится успехом
    while (!model.isSolved && oldUnresolvedCount != model.totalUnresolvedCount) {
      oldUnresolvedCount = model.totalUnresolvedCount()

      1 to model.columnNumber foreach (x => fillColumn(x - 1))
      1 to model.rowNumber foreach (y => fillRows(y - 1))
    }
  }

  /**
   * Заполнить столбцы
   * @param x Номер столбца (с нуля)
   */
  def fillColumn(x: Int) {
    fillLine(model.setCell(x, _: Int, _: Cell.Cell), model.rowNumber, model.horizonLine(x), model.getCell(x, _:Int))
  }

  /**
   * Заполнить строки
   * @param y Номер строки (с нуля)
   */
  def fillRows(y: Int) {
    fillLine(model.setCell(_: Int, y, _: Cell.Cell), model.columnNumber, model.verticalLine(y), model.getCell(_:Int, y))
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param setCell Метод установки значения по индексу
   * @param lineLength Размер линии
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param getLineData Метод получения данных из линии по индексу
   */
  def fillLine(setCell: (Int, Cell.Cell) => Unit, lineLength: Int, metadata: Array[Int], getLineData: (Int) => Cell.Cell) {

    def addDataToModel(compromise: List[Cell.Cell]) {
      0 to lineLength-1 filter (getLineData(_) == Cell.NOT_KNOWN) foreach(i => setCell(i, compromise(i)))
    }

    /**
     * Схлопывание линий в одну. Если во всех вариантах какие-либо ячейки совпадают,
     * то результирующая линия будет содержать эти значения. Иначе в ячейку попадет Cell.NOT_KNOWN
     * @param line1 Линия 1
     * @param line2 Линия 2
     * @return Сезультат схлопывания
     */
    def reduceLines(line1: List[Cell.Cell], line2: List[Cell.Cell]): List[Cell.Cell] = {
      val result = Array.fill[Cell.Cell](lineLength)(Cell.NOT_KNOWN)

      0 to lineLength -1 filter ((i) => line1(i) == line2(i)) foreach(i => result(i) = line1(i))
      result.toList
    }

    /**
     * Проверка, не противоречит ли предлагаемое значение текущим данным
     * @param supposeLine Предлагаемая линия
     * @return true, если вариант приемлим
     */
    def notCompatibleToCurrentData(supposeLine: List[Cell.Cell]): Boolean = {
      0 to lineLength-1 forall (i => getLineData(i) == Cell.NOT_KNOWN || getLineData(i) == supposeLine(i))
    }

    // Все возможные способы заполнения:
    val lines: Array[List[Cell.Cell]] = fitRemainder(lineLength, metadata).filter(notCompatibleToCurrentData)

    addDataToModel(lines.reduce(reduceLines))
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
