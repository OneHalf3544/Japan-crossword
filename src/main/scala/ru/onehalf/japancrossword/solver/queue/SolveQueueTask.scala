package ru.onehalf.japancrossword.solver.queue

import ru.onehalf.japancrossword.model.{Cell, Line}
import ru.onehalf.japancrossword.solver.LineSolver

/**
 * Задание для подбора строки
 * <p/>
 * <p/>
 * Created: 23.05.13 22:37
 * <p/>
 * @param metadata Данные по количестку закрашенных клеток в линии
 * @param line Подбираемая линия
 * @param solverType Решатель, которым еще не перебиралась данная строка
 * @param remindingCells Число ячеек, которые были не подобраны на момент создания задачи.
 *                       (Чтобы не гонять одного и того же решателя по одинаковым исходным данным)
 * @author OneHalf
 */
case class SolveQueueTask(metadata: Array[Int], line: Line, solverType: LineSolver, remindingCells: Int) {

  def this(metadata: Array[Int], line: Line, solverType: LineSolver) =
    this(metadata: Array[Int], line: Line, solverType: LineSolver, line.toList.count(_ == Cell.NOT_KNOWN))

  override def toString = "Task[" + metadata.mkString(",") + ", NotKnownCount:  " + line.notKnownCount + ", solver:" + solverType + "]"
}
