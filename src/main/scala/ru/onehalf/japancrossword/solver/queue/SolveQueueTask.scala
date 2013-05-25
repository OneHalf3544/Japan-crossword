package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.LineTrait
import ru.onehalf.japancrossword.solver.LineSolver

/**
 * <p/>
 * <p/>
 * Created: 23.05.13 22:37
 * <p/>
 * @author OneHalf
 */
case class SolveQueueTask(metadata: Array[Int], line: LineTrait, solverType: LineSolver )
