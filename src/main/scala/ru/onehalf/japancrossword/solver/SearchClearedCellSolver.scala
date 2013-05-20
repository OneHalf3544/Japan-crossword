package ru.onehalf.japancrossword.solver

import concurrent._
import ru.onehalf.japancrossword.model.{LineTrait, JapanCrosswordModel, Line}
import ru.onehalf.japancrossword.model.Cell._
import ExecutionContext.Implicits.global

/**
 * Поиск пустых ячеек
 * <p/>
 * <p/>
 * Created: 17.05.13 0:25
 * <p/>
 * @author OneHalf
 */
class SearchClearedCellSolver(model: JapanCrosswordModel) extends Solver(model) {

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    println("SearchClearedCell solve start")

    future {
      do {
        (0 to model.rowNumber - 1).foreach(v => {
          val line = new Line(v, Orientation.HORIZONTAL, model)
          addDataToModel(fillRow(v, line), line)
        })
        (0 to model.columnNumber - 1).foreach(v => {
          val line = new Line(v, Orientation.VERTICAL, model)
          addDataToModel(fillColumn(v, line), line)
        })
        Thread.sleep(100)
        // todo сделать нормальное условие выхода
      } while (!model.isSolved)
      println("SearchClearedCell solve end")
    }.onFailure{
      case e: Exception => println(e.printStackTrace())
    }
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать Cell.NOT_KNOWN значения
   */
  def fillLine(metadata: Array[Int], currentData: LineTrait): List[Cell] = {
    val stat: List[(Cell, Int)] = currentData.toList.foldLeft(List.empty[(Cell, Int)])(countCellTypes)

    if (stat.filter(_._1 == NOT_KNOWN).forall(_._2 > metadata.min)) {
      // Нечего заполнять
      return currentData.toList
    }

    val indexes: List[Int] = stat.scanLeft(0)((res, o) => o._2 + res)
    var preResult = currentData.toList

    for (i <- stat.indices) {

      def prevIsCleared: Boolean = !stat.isDefinedAt(i - 1) || stat(i - 1)._1 == CLEARED
      def nextIsCleared: Boolean = !stat.isDefinedAt(i + 1) || stat(i + 1)._1 == CLEARED
      def isNotKnown: Boolean = stat(i)._1 == NOT_KNOWN && stat(i)._2 < metadata.min

      if (prevIsCleared && isNotKnown && nextIsCleared) {
        val length = stat(i)._2
        preResult = preResult.patch(indexes(i), List.fill(length)(CLEARED), length)
      }
    }

    val logStat = preResult.foldLeft(List.empty[(Cell, Int)])(countCellTypes)
    if (stat.size != logStat.size) {
      println("stat:   " + stat)
      println("result: " + logStat)
      println("indexes " + indexes)
    }
    preResult
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
}
