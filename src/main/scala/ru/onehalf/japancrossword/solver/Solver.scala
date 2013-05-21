package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.{LineTrait, JapanCrosswordModel, Line, Cell}
import ru.onehalf.japancrossword.model.Cell._

/**
 * <p/>
 * <p/>
 * Created: 12.05.13 23:24
 * <p/>
 * @author OneHalf
 */
abstract class Solver(model: JapanCrosswordModel) {

  /**
   * Запуск решения кроссворда
   */
  def solve()

  /**
   * Копируем данные из массива в модель
   * @param variant
   * @param line
   */
  def addDataToModel(variant: List[Cell.Cell], line: Line) {
    0 to variant.size-1 filter (line(_) == Cell.NOT_KNOWN) foreach(i => line(i) = variant(i))
  }

  /**
   * Заполнить столбец
   * @param x Номер столбца (с нуля)
   */
  def fillColumn(x: Int, currentData: LineTrait) = {
    fillLine(model.horizonLine(x), currentData)
  }

  /**
   * Заполнить строку
   * @param y Номер строки (с нуля)
   */
  def fillRow(y: Int, currentData: LineTrait) = {
    fillLine(model.verticalLine(y), currentData)
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  def fillLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = {
    if (countStat(currentData.toList.filterNot(_ == NOT_KNOWN)).count(_._1 == FILLED) < metadata.size) {
      return fillSubLine(metadata, currentData)
    }

    val sublists = divideToSublists(currentData, countStat(currentData))
    assert(sublists.size == metadata.size)
    sublists.indices map (v => fillSubLine(Array(metadata(v)), sublists(v))) reduceLeft((a, b) => a ::: b)
  }


  def divideToSublists(line: LineTrait, stat: List[(Cell.Cell, Int)]): List[LineTrait] = {
    val statIndicies = indicesForStat(stat)
    var hasFilledCell = false

    for (i <- stat.indices) {
      if (stat(i)._1 == CLEARED && hasFilledCell) {
        val splitIndex = statIndicies(i)
        return line.dropRight(line.size - splitIndex) +: divideToSublists(line.drop(splitIndex), stat.drop(i))
      }
      if (stat(i)._1 == FILLED) {
        hasFilledCell = true
      }
    }
    List(line)
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell]

  /**
   * Схлопывание строки в одну. Если значение в списках не совпадают,
   * то в результате по соответсвующему индексу будет Cell.NOT_KNOWN.
   * В случае совпаднения значений, содержимое попадет в результирующий список
   * @param line1 Строка 1
   * @param line2 Строка 2
   * @return Результат объединения
   */
  def reduceLines(line1: List[Cell.Cell], line2: List[Cell.Cell]): List[Cell.Cell] = {
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
  def compatibleToCurrentData(currentData: LineTrait, supposeLine: List[Cell.Cell]): Boolean = {
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

  /**
   *
   * @param currentData
   * @return
   */
  def countStat(currentData: List[Cell.Cell]): List[(Cell.Cell, Int)] = {
    currentData.toList.foldLeft(List.empty[(Cell, Int)])(countCellTypes)
  }

  /**
   *
   * @param currentData
   * @return
   */
  def countStat(currentData: LineTrait): List[(Cell.Cell, Int)] = {
    countStat(currentData.toList)
  }

  def countCellTypes(a: List[(Cell, Int)], cell: Cell): List[(Cell, Int)] = {
    if (a.isEmpty) {
      return List((cell, 1))
    }
    if (a.last._1 == cell) {
      return a.init :+ (cell, a.last._2 + 1)
    }
    a :+ (cell, 1)
  }

  def indicesForStat(stat: List[(Cell.Cell, Int)]): List[Int] = stat.scanLeft(0)((res, o) => o._2 + res)

}
