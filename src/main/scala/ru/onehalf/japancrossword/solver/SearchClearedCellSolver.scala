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
          fillRow(v, line)
        })
        (0 to model.columnNumber - 1).foreach(v => {
          val line = new Line(v, Orientation.VERTICAL, model)
          fillColumn(v, line)
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

    List.empty
  }
}
