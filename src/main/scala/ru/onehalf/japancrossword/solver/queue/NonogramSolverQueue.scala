package ru.onehalf.japancrossword.solver.queue

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import com.typesafe.scalalogging.StrictLogging
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import ru.onehalf.japancrossword.model.line.{LineImpl, LineOfModel}
import ru.onehalf.japancrossword.solver._

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
  tasksQueue.add(new StatisticsPill(0, 0))

  private[queue] val splitter = new LineSplitter(this)
  private val logPeriod = Duration.ofSeconds(10)
  private val tasksProcessed = new AtomicLong(0)
  private val cellsProcessed = new AtomicLong(0)

  def startThread(start: Option[(String, Instant)] = None) {
    Future {
      logger.info(queueName + " started")

      while (!tasksQueue.isEmpty && !Thread.currentThread().isInterrupted) {
        val task = tasksQueue.take
        processTask(task)
        tasksProcessed.incrementAndGet()
        cellsProcessed.addAndGet(Option(task.line).map(_.size.toLong).getOrElse(0))
      }
      logger.info(s"end: $queueName empty")

    }.onComplete({
      case Failure(e) => logger.warn("error", e)
      case _ if start.isDefined => logger.info(
        s"nonogram '${start.get._1}' solved. time: ${Duration.between(start.get._2, Instant.now())}")
      case _ =>
    })
  }

  private def processTask(task: SolveQueueTask): Unit = {
    logger.trace(s"$queueName: task=$task")
    task match {
      case statTask: StatisticsPill => printStatsToLog(statTask)

      case SolveQueueTask(line, solver, _) if line.metadata.isEmpty =>
        val oldData = line.toList
        modelSolver.addDataToModel(LineImpl.empty(line.size), line)

      case SolveQueueTask(line, _, _) if line.isSolved =>
        logger.trace(s"line was solved: $line")

      case SolveQueueTask(line, solver, remindingCells) if splitter.splitLine(line, solver) =>
        logger.trace("line was split")

      case SolveQueueTask(line, solver, remindingCells) =>
        val solvedLine = solver.fillLine(LineImpl.copy(line))
        modelSolver.addDataToModel(solvedLine, line)
        if (!line.isSolved)
        // reenqueue line.
          this ! new SolveQueueTask(line, solver)
    }
  }

  def !(task: SolveQueueTask) {
    this.tasksQueue.add(task)
  }

  def enqueueLineForFastSolver(line: LineOfModel) {

    List(SearchOverlapsSolver, SearchClearedCellSolver).foreach(solver => {
      this ! SolveQueueTask(line, solver, Int.MaxValue)
    })

    this ! SolveQueueTask(line, BorderSolver, Int.MaxValue)
    this ! SolveQueueTask(line.reverse(), BorderSolver, Int.MaxValue)
  }

  private def printStatsToLog(pillTask: StatisticsPill): Unit = {
    val duration = Duration.between(pillTask.lastStatTime, Instant.now())

    if (tasksQueue.isEmpty || duration.compareTo(logPeriod) >= 0) {
      val taskProcessed = tasksProcessed.getAndSet(0)
      val cellProcessed = cellsProcessed.getAndSet(0)
      val durationPerTask = if (taskProcessed == 0) Duration.ZERO else duration.dividedBy(taskProcessed)
      val averageLength = if (taskProcessed == 0) 0 else cellProcessed / taskProcessed

      logger.info(s"$queueName: queue contains ${tasksQueue.size()} tasks")
      logger.info(s"$queueName: ${pillTask.circle} recent circle took $duration for $taskProcessed tasks ($durationPerTask per task)")
      logger.info(s"$queueName: average line size in the circle: $averageLength")
      if (!tasksQueue.isEmpty) {
        this ! new StatisticsPill(taskProcessed, cellProcessed)
      }
    } else {
      this ! new StatisticsPill(
        pillTask.lastProcessedCount,
        pillTask.lastCellsProcessed,
        pillTask.circle + 1,
        pillTask.lastStatTime
      )
    }
  }

  class StatisticsPill(val lastProcessedCount: Long,
                       val lastCellsProcessed: Long,
                       val circle: Int = 1,
                       val lastStatTime: Instant = Instant.now())
    extends SolveQueueTask(null, null, 0) {

    override def toString: String = getClass.getSimpleName
  }
}

