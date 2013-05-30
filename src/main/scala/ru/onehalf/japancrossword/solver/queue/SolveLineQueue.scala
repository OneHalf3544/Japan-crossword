package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.{LineImpl, Line, JapanCrosswordModel}
import ru.onehalf.japancrossword.solver._
import ru.onehalf.japancrossword.model.Cell._
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * <p/>
 * <p/>
 * Created: 23.05.13 22:39
 * <p/>
 * @author OneHalf
 */
class SolveLineQueue(model: JapanCrosswordModel, queueName: String) {

  def this(model: JapanCrosswordModel) = this(model, "dummy")

  // todo При вытаскивании задачи из очереди можно использовать какую-ниюудь
  // систему приоритетов и/млм учитывать, была ли линия изменена после последнего подбора

  val queue: BlockingQueue[SolveQueueTask] = new LinkedBlockingQueue[SolveQueueTask]

  val splitter = new LineSplitter(this)

  def startThread() {
    future {
      println(queueName + " started")

      while (!(model.isSolved || Thread.currentThread().isInterrupted)) {
        val task = queue.take()
        println("task: " + task + " for " + queueName)

        task match {

          case SolveQueueTask(_, line, _, _) if line.notKnownCount == 0 => {
            println("line solved")
          }

          case SolveQueueTask(metadata, line, _, _) if metadata.isEmpty => {
            addDataToModel(List.fill(line.size)(CLEARED), line)
          }

          case SolveQueueTask(metadata, line, solver, remindingCells) if (splitter.splitLine(metadata, line, solver)) => {
            println("строка была разделена")
          }

          case SolveQueueTask(metadata, line, solver, remindingCells) => {
            addDataToModel(solver.fillLine(metadata, line), line)
            if (!line.forall(_ != NOT_KNOWN))
              this ! new SolveQueueTask(metadata, line, solver)
          }
        }
      }
      println("end: " + queueName + " empty")
    }.onFailure({
      case e: Exception => e.printStackTrace()
    })
  }

  def ! (task: SolveQueueTask) {
    this.queue.add(task)
  }

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    val fastQueue = new SolveLineQueue(model, "fast queue")
    val columnQueue = new SolveLineQueue(model, "column queue")
    val rowQueue = new SolveLineQueue(model, "row queue")

    // Добавляем все линии в очредь
    val columns =
      (0 to model.columnNumber - 1).map(v => (new LineImpl(v, Orientation.VERTICAL, model), model.horizonLine(v)))

    val rows =
      (0 to model.rowNumber - 1).map(v => (new LineImpl(v, Orientation.HORIZONTAL, model), model.verticalLine(v)))

    columns.sortBy(_._2.size).foreach(v => {
      fastQueue.enqueueLineForFastSolver(v)
      columnQueue ! new SolveQueueTask(v._2, v._1, VariantsEnumerationSolver, Int.MaxValue)
    })
    rows.sortBy(_._2.size).foreach(v => {
      fastQueue.enqueueLineForFastSolver(v)
      rowQueue ! new SolveQueueTask(v._2, v._1, VariantsEnumerationSolver, Int.MaxValue)
    })

    for(i <- 1 to 4) {
      columnQueue.startThread()
      rowQueue.startThread()
    }
    fastQueue.startThread()
  }

  def enqueueLineForFastSolver(v: (Line, Array[Int])) {

    List(FastPreSolver, SearchClearedCellSolver).foreach(s => {
      this ! new SolveQueueTask(v._2, v._1, s, Int.MaxValue)
    })

    this ! new SolveQueueTask(v._2, v._1, BorderSolver, Int.MaxValue)
    this ! new SolveQueueTask(v._2.reverse, v._1.reverse(), BorderSolver, Int.MaxValue)
  }

  /**
   * Копируем данные из массива в модель
   * @param variant Вариант расположения ячеек в линии
   * @param line Кусочек модели, в которую нужно скопировать предлагаемые значения
   */
  def addDataToModel(variant: List[Cell], line: Line) {
    0 to variant.size-1 filter (i => line(i) == NOT_KNOWN && variant(i) != NOT_KNOWN) foreach(i => line(i) = variant(i))
  }

}
