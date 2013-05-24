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
  // При возможности разделить линию на части,  добавлять в очередь две задачи,
  // по одной на каждую часть линии


  def act() {
    receive {
      case SolveQueueTask(metadata, line) if metadata.isEmpty => {
        addDataToModel(List.fill(line.size)(Cell.CLEARED), line)
      }
      case SolveQueueTask(Array(chunkLength), line) =>
        addDataToModel(fillSubLine(Array(chunkLength), line), line)

      case SolveQueueTask(metadata, line) => {
        if (!splitLine(metadata, line)) {
          addDataToModel(fillSubLine(metadata, line), line)
        }
      }
    }
  }

  /**
   * Запуск решения кроссворда
   */
  def solve() {
    act()

    // Добавляем все линии в очредь
    (0 to model.columnNumber - 1).toList.sortBy(model.horizonLine(_).size).par.foreach(v => {
      val line = new Line(v, Orientation.VERTICAL, model)
      this ! new SolveQueueTask(model.horizonLine(v), line)
      this ! new SolveQueueTask(model.horizonLine(v).reverse, line.reverse())
    })
    (0 to model.rowNumber - 1).toList.sortBy(model.verticalLine(_).size).par.foreach(v => {
      val line = new Line(v, Orientation.HORIZONTAL, model)
      this ! new SolveQueueTask(model.verticalLine(v), line)
      this ! new SolveQueueTask(model.verticalLine(v).reverse, line.reverse())
    })

  }

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать NOT_KNOWN значения
   */
  def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell.Cell] = {
    FastPreSolver
    BorderSolver.fillSubLine(metadata, currentData)
    SearchClearedCellSolver.fillSubLine(metadata, currentData)
    VariantsEnumerationSolver.fillSubLine(metadata, currentData)
  }

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
   * @return Предполагаемый вариант линии. Может содержать NOT_KNOWN значения
   */
  def splitLine(metadata: Array[Int], currentData: LineTrait): Boolean = {

    if (countStat(currentData.toList.filterNot(_ == NOT_KNOWN)).count(_._1 == FILLED) < metadata.size) {
      return false
    }

    val sublists = divideToSublists(currentData, countStat(currentData))
    assert(sublists.size == metadata.size, "size not equals: %s and %s".format(sublists, metadata.mkString("[", ",", "]")))
    sublists.indices map (v => this ! new SolveQueueTask(Array(metadata(v)), sublists(v)))

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
