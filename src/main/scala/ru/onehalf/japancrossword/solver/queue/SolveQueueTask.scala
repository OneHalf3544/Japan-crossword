package ru.onehalf.japancrossword.solver.queue

import java.util.Objects

import com.typesafe.scalalogging.StrictLogging
import ru.onehalf.japancrossword.model.Cell
import ru.onehalf.japancrossword.model.line.LineOfModel
import ru.onehalf.japancrossword.solver.LineSolver

/**
  * Задание для подбора строки
  *
  * @param line Подбираемая линия
  * @param solverType Решатель, которым еще не перебиралась данная строка
  * @param remainingCells Число ячеек, которые были не подобраны на момент создания задачи.
  *                       (Чтобы не гонять одного и того же решателя по одинаковым исходным данным)
  * @since 23.05.13 22:37
  * @author OneHalf
  */
case class SolveQueueTask(line: LineOfModel, solverType: LineSolver, remainingCells: Int) extends StrictLogging {

  def this(line: LineOfModel, solverType: LineSolver) {
    this (line: LineOfModel, solverType: LineSolver, line.notKnownCount)
  }


  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[SolveQueueTask]) {
      return false
    }
    val o = obj.asInstanceOf[SolveQueueTask]
    line == o.line
  }

  override def hashCode(): Int = Objects.hash(line)

  override def toString: String = s"Task['$line', $solverType]"
}
