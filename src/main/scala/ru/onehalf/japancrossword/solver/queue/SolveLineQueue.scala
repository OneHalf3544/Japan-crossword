package ru.onehalf.japancrossword.solver.queue

import actors.Actor
import ru.onehalf.japancrossword.model.{Line, Cell, LineTrait, JapanCrosswordModel}
import ru.onehalf.japancrossword.solver._
import ru.onehalf.japancrossword.model.Cell._

/**
 * <p/>
 * <p/>
 * Created: 23.05.13 22:39
 * <p/>
 * @author OneHalf
 */
class SolveLineQueue(model: JapanCrosswordModel) extends LineSolver with Actor {

  // todo Вместо итерирования по индексам сделать очередь задач
  // т.е. взять все строки и добавить в одну коллекцию, потом выбирать по
  // одной и производить вычисления.
  // При вытаскивании задачи из очереди можно использовать какую-ниюудь
  // систему приоритетов и/млм учитывать, была ли линия изменена после последнего подбора
  // Полностью подобранные линии удалять из очереди.
  // Для линий, подобранныхлиных на концах, можно возвращать в очередь линии меньшей длины.
  // При возможности разделить линию на части,  добавлять в очередь несколько задач,
  // по одной на каждую часть линии


  def act() {
    while (!model.isSolved) {
      receive {
        case SolveQueueTask(metadata, line, _) if metadata.isEmpty => {
          addDataToModel(List.fill(line.size)(Cell.CLEARED), line)
        }

        case SolveQueueTask(metadata, line, solver) => {
          if (!splitLine(metadata, line, solver)) {
            addDataToModel(solver.fillLine(metadata, line), line)
            if (!line.forall(_ != NOT_KNOWN))
              this ! new SolveQueueTask(metadata, line, solver)
          }
        }
      }
    }
  }

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    start()

    // Добавляем все линии в очредь
    (0 to model.columnNumber - 1).toList.sortBy(model.horizonLine(_).size).par.foreach(v => {
      val line = new Line(v, Orientation.VERTICAL, model)
      enqueueLineForAllSolver(line, model.horizonLine(v))
    })
    (0 to model.rowNumber - 1).toList.sortBy(model.verticalLine(_).size).par.foreach(v => {
      val line = new Line(v, Orientation.HORIZONTAL, model)
      enqueueLineForAllSolver(line, model.verticalLine(v))
    })
  }

  def enqueueLineForAllSolver(line: Line, metadata: Array[Int]) {
    List(FastPreSolver, BorderSolver, SearchClearedCellSolver, VariantsEnumerationSolver).foreach(s => {
      this ! new SolveQueueTask(metadata, line, s)
      this ! new SolveQueueTask(metadata.reverse, line.reverse(), s)
    })
  }

  // todo Переделать иерархию классов, убрать это переопределение
  def fillLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = null

  /**
   * Копируем данные из массива в модель
   * @param variant
   * @param line
   */
  def addDataToModel(variant: List[Cell], line: LineTrait) {
    0 to variant.size-1 filter (line(_) == Cell.NOT_KNOWN) foreach(i => line(i) = variant(i))
  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return true, если линия разделена
   */
  def splitLine(metadata: Array[Int], currentData: LineTrait, solver: LineSolver): Boolean = {

    if (countStat(currentData.toList.filterNot(_ == NOT_KNOWN)).count(_._1 == FILLED) < metadata.size) {
      return false
    }

    val sublists = divideToSublists(currentData, countStat(currentData))
    assert(sublists.size == metadata.size, "size not equals: %s and %s".format(sublists, metadata.mkString("[", ",", "]")))
    sublists.indices map (v => this ! new SolveQueueTask(Array(metadata(v)), sublists(v), solver))

    true
  }


  def divideToSublists(line: LineTrait, stat: List[(Cell, Int)]): List[LineTrait] = {
    val statIndicies = indicesForStat(stat)
    var hasFilledCell = false

    for (i <- stat.indices) {
      if (stat(i)._1 == CLEARED && hasFilledCell && (stat.drop(i).exists(_._1 == FILLED))) {
        val splitIndex = statIndicies(i)
        return line.dropRight(line.size - splitIndex) +: divideToSublists(line.drop(splitIndex), stat.drop(i))
      }
      if (stat(i)._1 == FILLED) {
        hasFilledCell = true
      }
    }
    List(line)
  }

}
