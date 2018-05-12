package ru.onehalf.japancrossword.solver

import java.time.Instant

import com.typesafe.scalalogging.StrictLogging
import queue.{NonogramSolverQueue, SolveQueueTask}
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.Orientation._
import ru.onehalf.japancrossword.model.line.{Line, LineOfModel}

/**
  * Solves a puzzle.
  *
  * @since 05.06.13 22:47
  * @author OneHalf
  */
class ModelSolver(model: JapanCrosswordModel) extends StrictLogging {

  private val fastQueue: NonogramSolverQueue = new NonogramSolverQueue(model, "fast", this)
  private val columnQueue: NonogramSolverQueue = new NonogramSolverQueue(model, "column", this)
  private val rowQueue: NonogramSolverQueue = new NonogramSolverQueue(model, "row", this)

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    logger.info(s"start solving nonogram '${model.name}'")
    // Добавляем все линии в очредь
    val columns = (0 until model.columnNumber).map(v => model.getColumnLine(v))

    val rows = (0 until model.rowNumber).map(v => model.getRowLine(v))

    columns.sortBy(_.metadata.size).foreach(enqueue(_, VERTICAL))
    rows   .sortBy(_.metadata.size).foreach(enqueue(_, HORIZONTAL))

    fastQueue.startThread(Some((model.name, Instant.now())))
    for(_ <- 1 to 3) {
      columnQueue.startThread()
      rowQueue   .startThread()
    }
  }

  def enqueue(line: LineOfModel, orientation: Orientation) {
    fastQueue.enqueueLineForFastSolver(line)

    (if (orientation == VERTICAL) columnQueue else rowQueue ) !
      SolveQueueTask(line, VariantsEnumerationSolver, Int.MaxValue)
  }

  /**
   * Копируем данные из массива в модель
   * @param solvingResult Вариант расположения ячеек в линии
   * @param lineInModel Кусочек модели, в которую нужно скопировать предлагаемые значения
   */
  def addDataToModel(solvingResult: Line, lineInModel: LineOfModel) {
    for (i <- solvingResult.indexes) {
      if (solvingResult(i) != NOT_KNOWN) {
        if (lineInModel(i) == NOT_KNOWN) {
          lineInModel(i) = solvingResult(i)
//          lineInModel.orientation match {
//            case HORIZONTAL => enqueue(model.getColumnLine(i), VERTICAL)
//            case VERTICAL => enqueue(model.getRowLine(i), HORIZONTAL)
//          }
        } else {
          assert(lineInModel(i) == solvingResult(i))
        }
      }
    }
  }
}
