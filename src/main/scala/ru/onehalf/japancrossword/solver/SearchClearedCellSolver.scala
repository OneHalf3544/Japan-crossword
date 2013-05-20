package ru.onehalf.japancrossword.solver

import concurrent._
import ru.onehalf.japancrossword.model.{LineTrait, JapanCrosswordModel, Line}
import ru.onehalf.japancrossword.model.Cell._
import ExecutionContext.Implicits.global
import java.util.logging.Logger

/**
 * Поиск пустых ячеек
 * <p/>
 * <p/>
 * Created: 17.05.13 0:25
 * <p/>
 * @author OneHalf
 */
class SearchClearedCellSolver(model: JapanCrosswordModel) extends Solver(model) {

  val logger = Logger.getLogger(getClass.getName)

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

    val indexes: List[Int] = stat.scanLeft(0)((res, o) => o._2 + res)
    var preResult = currentData.toList

    if (stat.filter(_._1 == FILLED).corresponds(metadata)((a, b) => a._2 == b)) {
      // Все уже решено
      return preResult.map(v => if (v == NOT_KNOWN) CLEARED else v)
    }

    for (i <- stat.indices) {

      def prevIsCleared: Boolean = !stat.isDefinedAt(i - 1) || stat(i - 1)._1 == CLEARED
      def nextIsCleared: Boolean = !stat.isDefinedAt(i + 1) || stat(i + 1)._1 == CLEARED
      def isNotKnown: Boolean = stat(i)._1 == NOT_KNOWN && stat(i)._2 < metadata.min

      if (prevIsCleared && isNotKnown && nextIsCleared) {
        val length = stat(i)._2
        preResult = preResult.patch(indexes(i), List.fill(length)(CLEARED), length)
      }
    }

    for (i <- stat.indices) {
      if (stat(i)._1 == FILLED && stat(i)._2 == metadata.max) {
        if (indexes.isDefinedAt(i - 1))
          preResult = preResult.updated(indexes(i) - 1, CLEARED)
        if (indexes.isDefinedAt(i + 2))
          preResult = preResult.updated(indexes(i + 1), CLEARED)
      }
    }

    if (metadata.size == 1) {
      val filledIndexes = preResult.indices.filter(preResult(_) == FILLED)
      if (filledIndexes.nonEmpty) {
        val minIndex = filledIndexes.max - metadata(0)
        val maxIndex = filledIndexes.min + metadata(0)

        preResult =
          List.fill[Cell](minIndex + 1)(CLEARED) :::
          preResult.slice(minIndex + 1, maxIndex) :::
          List.fill[Cell](preResult.size - maxIndex)(CLEARED)
      }
    }

    val logStat = preResult.foldLeft(List.empty[(Cell, Int)])(countCellTypes)
    if (stat.size != logStat.size) {
      logger.fine("stat:   " + stat)
      logger.fine("result: " + logStat)
      logger.fine("indexes " + indexes)
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
