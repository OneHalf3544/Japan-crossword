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

  // todo Вместо итерирования по индексам сделать очередь задач
  // т.е. взять все строки и добавить в одну коллекцию, потом выбирать по
  // одной и производить вычисления.
  // При вытаскивании задачи из очереди можно использовать какую-ниюудь
  // систему приоритетов и/млм учитывать, была ли линия изменена после последнего подбора
  // Полностью подобранные линии удалять из очереди.
  // Для линий, подобранныхлиных на концах, можно возвращать в очередь линии меньшей длины.
  // При возможности разделить линию на части,  добавлять в очередь несколько задач,
  // по одной на каждую часть линии

  val queue: BlockingQueue[SolveQueueTask] = new LinkedBlockingQueue[SolveQueueTask]

  val splitter = new LineSplitter(this)

  def start() {
    println("queue started")

    while (!(model.isSolved || Thread.currentThread().isInterrupted)) {
      val task = queue.take()
      println("task: " + task)

      task match {

        case SolveQueueTask(metadata, line, _, _) if metadata.isEmpty => {
          addDataToModel(List.fill(line.size)(CLEARED), line)
        }

        // todo реализовать пропуск задач, котоорые не изменились с момента прошлого решения
        case SolveQueueTask(metadata, line, solver, remindingCells) if (line.notKnownCount == remindingCells) => {
          this ! new SolveQueueTask(metadata, line, solver, remindingCells)
        }

        case SolveQueueTask(metadata, line, solver, remindingCells) if (!splitter.splitLine(metadata, line, solver)) => {
          addDataToModel(solver.fillLine(metadata, line), line)
          if (!line.forall(_ != NOT_KNOWN))
            this ! new SolveQueueTask(metadata, line, solver)
        }
        case _ => throw new IllegalStateException("unknown task")
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
    (0 to model.columnNumber - 1).toList.sortBy(model.horizonLine(_).size).foreach(v => {
      val line = new LineImpl(v, Orientation.VERTICAL, model)
      enqueueLineForAllSolver(line, model.horizonLine(v))
    })
    (0 to model.rowNumber - 1).toList.sortBy(model.verticalLine(_).size).foreach(v => {
      val line = new LineImpl(v, Orientation.HORIZONTAL, model)
      enqueueLineForAllSolver(line, model.verticalLine(v))
    })

    start()
  }

  def enqueueLineForAllSolver(line: LineImpl, metadata: Array[Int]) {
    this ! new SolveQueueTask(metadata, line, FastPreSolver, Int.MaxValue)
    this ! new SolveQueueTask(metadata, line, SearchClearedCellSolver, Int.MaxValue)

    List(BorderSolver, VariantsEnumerationSolver).foreach(s => {
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
