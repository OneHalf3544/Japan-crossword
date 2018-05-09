package ru.onehalf.japancrossword.solver.queue

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import com.typesafe.scalalogging.StrictLogging
import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.solver._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.Failure

/**
  * A solver for a nonogram.
  * Uses a BlockingQueue to determine which line will be processed next.
  *
  * @since 23.05.13 22:39
  * @author OneHalf
  */
class NonogramSolverQueue(model: Model, queueName: String, modelSolver: ModelSolver) extends StrictLogging {

  // todo use priority queue for put shorter lines to the beginning of queue.

  private val tasksQueue: BlockingQueue[SolveQueueTask] = new LinkedBlockingQueue[SolveQueueTask]

  private[queue] val splitter = new LineSplitter(this)

  def startThread() {
    Future {
      logger.info(queueName + " started")

      while (!(model.isSolved || Thread.currentThread().isInterrupted)) {
        val task = tasksQueue.take()
        logger.trace(s"task: $task for $queueName")

        task match {

          case SolveQueueTask(_, line, _, _) if line.isSolved =>
            logger.trace(s"line solved: $line")

          case SolveQueueTask(metadata, line, solver, _) if metadata.isEmpty =>
            val oldData = line.toList
            modelSolver.addDataToModel(oldData, List.fill(line.size)(Cleared), line)

          case SolveQueueTask(metadata, line, solver, remindingCells) if splitter.splitLine(metadata, line, solver) =>
            logger.trace("line split")

          case SolveQueueTask(metadata, line, solver, remindingCells) =>
            val oldData = line.toList
            modelSolver.addDataToModel(oldData, solver.fillLine(metadata, line), line)
            if (!line.isSolved())
              this ! new SolveQueueTask(metadata, line, solver)
        }
      }
      logger.info(s"end: $queueName empty")
    }.onComplete({
      case Failure(e) => logger.warn("error", e)
      case _ =>
    })
  }

  def ! (task: SolveQueueTask) {
    this.tasksQueue.add(task)
  }

  def enqueueLineForFastSolver(v: (Line, LineMetadata)) {

    List(SearchOverlapsSolver, SearchClearedCellSolver).foreach(s => {
      this ! SolveQueueTask(v._2, v._1, s, Int.MaxValue)
    })

    this ! SolveQueueTask(v._2, v._1, BorderSolver, Int.MaxValue)
    this ! SolveQueueTask(v._2.reverse, v._1.reverse(), BorderSolver, Int.MaxValue)
  }


}
