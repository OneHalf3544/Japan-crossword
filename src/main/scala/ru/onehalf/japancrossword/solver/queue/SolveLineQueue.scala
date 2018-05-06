package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.{Line, JapanCrosswordModel}
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
class SolveLineQueue(model: JapanCrosswordModel, queueName: String, modelSolver: ModelSolver) extends Logging {

  // todo При вытаскивании задачи из очереди можно использовать какую-ниюудь
  // систему приоритетов и/млм учитывать, была ли линия изменена после последнего подбора

  val queue: BlockingQueue[SolveQueueTask] = new LinkedBlockingQueue[SolveQueueTask]

  val splitter = new LineSplitter(this)

  def startThread() {
    Future {
      println(queueName + " started")

      while (!(model.isSolved || Thread.currentThread().isInterrupted)) {
        val task = queue.take()
        //println("task: " + task + " for " + queueName)

        task match {

          case SolveQueueTask(_, line, _, _) if line.notKnownCount == 0 => {
            //println("line solved")
          }

          case SolveQueueTask(metadata, line, solver, _) if metadata.isEmpty => {
            val oldData = line.toList
            modelSolver.addDataToModel(oldData, List.fill(line.size)(CLEARED), line)
          }

          case SolveQueueTask(metadata, line, solver, remindingCells) if splitter.splitLine(metadata, line, solver) => {
            //println("строка была разделена")
          }

          case SolveQueueTask(metadata, line, solver, remindingCells) => {
            val oldData = line.toList
            modelSolver.addDataToModel(oldData, solver.fillLine(metadata, line), line)
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

  def enqueueLineForFastSolver(v: (Line, Array[Int])) {

    List(SearchOverlapsSolver, SearchClearedCellSolver).foreach(s => {
      this ! SolveQueueTask(v._2, v._1, s, Int.MaxValue)
    })

    this ! SolveQueueTask(v._2, v._1, BorderSolver, Int.MaxValue)
    this ! SolveQueueTask(v._2.reverse, v._1.reverse(), BorderSolver, Int.MaxValue)
  }


}
