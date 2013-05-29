package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.{LineImpl, Line, JapanCrosswordModel}
import ru.onehalf.japancrossword.solver._
import ru.onehalf.japancrossword.model.Cell._
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}

/**
 * <p/>
 * <p/>
 * Created: 23.05.13 22:39
 * <p/>
 * @author OneHalf
 */
class SolveLineQueue(model: JapanCrosswordModel) {

  // todo При вытаскивании задачи из очереди можно использовать какую-ниюудь
  // систему приоритетов и/млм учитывать, была ли линия изменена после последнего подбора

  val queue: BlockingQueue[SolveQueueTask] = new LinkedBlockingQueue[SolveQueueTask]

  val splitter = new LineSplitter(this)

  def start() {
    println("queue started")

    while (!(model.isSolved || Thread.currentThread().isInterrupted)) {
      val task = queue.take()
      println("task: " + task)

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
    println("end: queue empty")
  }

  def ! (task: SolveQueueTask) {
    queue.add(task)
  }

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    // Добавляем все линии в очредь
    val list: List[(Line, Array[Int])] =
      (0 to model.columnNumber - 1).map(v => (new LineImpl(v, Orientation.VERTICAL, model), model.horizonLine(v))).toList :::
      (0 to model.rowNumber - 1).map(v => (new LineImpl(v, Orientation.HORIZONTAL, model), model.verticalLine(v))).toList

    // Мешаем вертикальные и горизонтальные линии в одну коллекцию
    list.sortBy(_._2.size).foreach(v => enqueueLineForAllSolver(v._1, v._2))

    start()
  }

  def enqueueLineForAllSolver(line: Line, metadata: Array[Int]) {

    List(FastPreSolver, SearchClearedCellSolver, VariantsEnumerationSolver).foreach(s => {
      this ! new SolveQueueTask(metadata, line, s, Int.MaxValue)
    })

    List(BorderSolver).foreach(s => {
      this ! new SolveQueueTask(metadata, line, s, Int.MaxValue)
      this ! new SolveQueueTask(metadata.reverse, line.reverse(), s, Int.MaxValue)
    })
  }

  /**
   * Копируем данные из массива в модель
   * @param variant
   * @param line
   */
  def addDataToModel(variant: List[Cell], line: Line) {
    0 to variant.size-1 filter (i => line(i) == NOT_KNOWN && variant(i) != NOT_KNOWN) foreach(i => line(i) = variant(i))
  }

}
