package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.{LineImpl, Cell, Line, JapanCrosswordModel}
import ru.onehalf.japancrossword.solver._
import ru.onehalf.japancrossword.model.Cell._
import java.util.concurrent.{LinkedBlockingDeque, BlockingQueue}

/**
 * <p/>
 * <p/>
 * Created: 23.05.13 22:39
 * <p/>
 * @author OneHalf
 */
class SolveLineQueue(model: JapanCrosswordModel) {

  // todo Вместо итерирования по индексам сделать очередь задач
  // т.е. взять все строки и добавить в одну коллекцию, потом выбирать по
  // одной и производить вычисления.
  // При вытаскивании задачи из очереди можно использовать какую-ниюудь
  // систему приоритетов и/млм учитывать, была ли линия изменена после последнего подбора
  // Полностью подобранные линии удалять из очереди.
  // Для линий, подобранныхлиных на концах, можно возвращать в очередь линии меньшей длины.
  // При возможности разделить линию на части,  добавлять в очередь несколько задач,
  // по одной на каждую часть линии

  val queue: BlockingQueue[SolveQueueTask] =
    new LinkedBlockingDeque[SolveQueueTask](model.columnNumber + model.rowNumber)

  val splitter = new LineSplitter(this)

  def start() {
    println("queue started")

    while (!model.isSolved && !Thread.currentThread().isInterrupted) {
      val task = queue.take()
      println("task: " + task)

      task match {

        case SolveQueueTask(metadata, line, _, _) if metadata.isEmpty => {
          addDataToModel(List.fill(line.size)(Cell.CLEARED), line)
        }

        case SolveQueueTask(metadata, line, solver, remindingCells) if (line.notKnownCount == remindingCells) => {
          this ! new SolveQueueTask(metadata, line, solver, remindingCells)
        }

        case SolveQueueTask(metadata, line, solver, remindingCells) if (!splitter.splitLine(metadata, line, solver)) => {
          addDataToModel(solver.fillLine(metadata, line), line)
          if (!line.forall(_ != NOT_KNOWN))
            this ! new SolveQueueTask(metadata, line, solver)
        }
        case _ => throw new IllegalStateException()
      }
    }
    println("end: queue empty")
  }

  def ! (task: SolveQueueTask) {
    queue.put(task)
  }

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    start()

    // Добавляем все линии в очредь
    (0 to model.columnNumber - 1).toList.sortBy(model.horizonLine(_).size).par.foreach(v => {
      val line = new LineImpl(v, Orientation.VERTICAL, model)
      enqueueLineForAllSolver(line, model.horizonLine(v))
    })
    (0 to model.rowNumber - 1).toList.sortBy(model.verticalLine(_).size).par.foreach(v => {
      val line = new LineImpl(v, Orientation.HORIZONTAL, model)
      enqueueLineForAllSolver(line, model.verticalLine(v))
    })
  }

  def enqueueLineForAllSolver(line: LineImpl, metadata: Array[Int]) {
    List(FastPreSolver, BorderSolver, SearchClearedCellSolver, VariantsEnumerationSolver).foreach(s => {
      this ! new SolveQueueTask(metadata, line, s)
      this ! new SolveQueueTask(metadata.reverse, line.reverse(), s)
    })
  }

  /**
   * Копируем данные из массива в модель
   * @param variant
   * @param line
   */
  def addDataToModel(variant: List[Cell], line: Line) {
    0 to variant.size-1 filter (line(_) == Cell.NOT_KNOWN) foreach(i => line(i) = variant(i))
  }

}
