package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.JapanCrosswordModel
import ru.onehalf.japancrossword.solver._
import ru.onehalf.japancrossword.model.Cell._
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import com.typesafe.scalalogging.StrictLogging
import ru.onehalf.japancrossword.model.line.{Line, LineImpl, LineOfModel}

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure

/**
  * A solver for a nonogram.
  * Uses a BlockingQueue to determine which line will be processed next.
  *
  * @since 23.05.13 22:39
  * @author OneHalf
  */
class NonogramSolverQueue(model: JapanCrosswordModel, queueName: String, modelSolver: ModelSolver) extends StrictLogging {

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

          case SolveQueueTask(line, _, _) if line.isSolved =>
            logger.trace(s"line solved: $line")

          case SolveQueueTask(line, solver, _) if line.metadata.isEmpty =>
            val oldData = line.toList
            modelSolver.addDataToModel(oldData, LineImpl.empty(line.size), line)

          case SolveQueueTask(line, solver, remindingCells) if splitter.splitLine(line, solver) =>
            logger.trace("line split")

          case SolveQueueTask(line, solver, remindingCells) =>
            val oldData = line.toList
            modelSolver.addDataToModel(oldData, solver.fillLine(line), line)
            if (!line.isSolved)
              this ! new SolveQueueTask(line, solver)
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

  def enqueueLineForFastSolver(line: LineOfModel) {

    List(SearchOverlapsSolver, SearchClearedCellSolver).foreach(solver => {
      this ! SolveQueueTask(line, solver, Int.MaxValue)
    })

    this ! SolveQueueTask(line, BorderSolver, Int.MaxValue)
    this ! SolveQueueTask(line.reverse(), BorderSolver, Int.MaxValue)
  }


}
