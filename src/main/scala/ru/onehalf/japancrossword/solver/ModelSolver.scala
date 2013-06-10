package ru.onehalf.japancrossword.solver

import queue.{SolveQueueTask, NonogramSolverQueue}
import ru.onehalf.japancrossword.model.{LineMetadata, Line, Model}
import ru.onehalf.japancrossword.model.Cell
import ru.onehalf.japancrossword.model.Orientation._

/**
  * Solves a puzzle.
  *
  * @since 05.06.13 22:47
  * @author OneHalf
  */
class ModelSolver(model: Model) {

  private val fastQueue: NonogramSolverQueue = new NonogramSolverQueue(model, "fast", this)
  private val columnQueue: NonogramSolverQueue = new NonogramSolverQueue(model, "column", this)
  private val rowQueue: NonogramSolverQueue = new NonogramSolverQueue(model, "row", this)

  /**
   * Запуск решения кроссворда
   */
  def solve() {

    // Добавляем все линии в очредь
    val columns = (0 until model.columnNumber).map(v => model.getColumnLine(v))

    val rows = (0 until model.rowNumber).map(v => model.getRowLine(v))

    columns.sortBy(_._2.length).foreach(enqueue(_, VERTICAL))
    rows   .sortBy(_._2.length).foreach(enqueue(_, HORIZONTAL))

    fastQueue.startThread()
    for(i <- 1 to 3) {
      columnQueue.startThread()
      rowQueue   .startThread()
    }
  }

  def enqueue(v: (Line, LineMetadata), orientation: Orientation) {
    fastQueue.enqueueLineForFastSolver(v)

    (if (orientation == VERTICAL) columnQueue else rowQueue ) !
      SolveQueueTask(v._2, v._1, VariantsEnumerationSolver, Int.MaxValue)
  }

  /**
   * Копируем данные из массива в модель
   * @param variant Вариант расположения ячеек в линии
   * @param line Кусочек модели, в которую нужно скопировать предлагаемые значения
   */
  def addDataToModel(oldData: List[Cell], variant: List[Cell], line: Line) {
    variant.indices filter (i => line(i).isNotKnown && !variant(i).isNotKnown) foreach(i => {
      line(i) = variant(i)
/*      line.orientation match {
        case HORIZONTAL => enqueue(model.getColumnLine(i), VERTICAL)
        case VERTICAL   => enqueue(model.getRowLine(i),    HORIZONTAL)
      }*/
    })
  }
}
