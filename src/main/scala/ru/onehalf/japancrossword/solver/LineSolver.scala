package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.{Cell, Line}
import ru.onehalf.japancrossword.model.Cell.Cell

/**
 * <p/>
 * <p/>
 * Created: 24.05.13 23:09
 * <p/>
 * @author OneHalf
 */
trait LineSolver {

  def fillLine(metadata: Array[Int], currentData: Line): List[Cell.Cell]

  def indicesForStat(stat: List[(Cell, Int)]): List[Int] = stat.scanLeft(0)((res, o) => o._2 + res)

  /**
   * Схлопывание строки в одну. Если значение в списках не совпадают,
   * то в результате по соответсвующему индексу будет NOT_KNOWN.
   * В случае совпаднения значений, содержимое попадет в результирующий список
   * @param line1 Строка 1
   * @param line2 Строка 2
   * @return Результат объединения
   */
  def reduceLines(line1: List[Cell], line2: List[Cell]): List[Cell] = {
    assert(line1.size == line2.size, "lines: " + line1.size + ", " + line2.size)
    val lineLength = line1.size

    val result = Array.fill[Cell](lineLength)(NOT_KNOWN)

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
  def compatibleToCurrentData(currentData: Line, supposeLine: List[Cell]): Boolean = {
    compatibleToCurrentData(currentData.toList, supposeLine)
  }

  /**
   * Проверка, не противоречит ли предлагаемое значение текущим данным
   * @param currentData Текущие данные в модели
   * @param supposeLine Предлагаемая линия (Может содержать NOT_KNOWN)
   * @return true, если вариант приемлим
   */
  def compatibleToCurrentData(currentData: List[Cell], supposeLine: List[Cell]): Boolean = {

    def cellIsCompatible(i: Int): Boolean = {
      currentData(i) == supposeLine(i) || currentData(i) == NOT_KNOWN || supposeLine(i) == NOT_KNOWN
    }

    assert(currentData.size == supposeLine.size)
    0 to supposeLine.size-1 forall (cellIsCompatible(_))
  }

  /**
   *
   * @param currentData
   * @return
   */
  def countStat(currentData: List[Cell]): List[(Cell, Int)] = {
    currentData.toList.foldLeft(List.empty[(Cell, Int)])(countCellTypes)
  }

  /**
   *
   * @param currentData
   * @return
   */
  def countStat(currentData: Line): List[(Cell, Int)] = {
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

  override def toString = getClass.getSimpleName
}