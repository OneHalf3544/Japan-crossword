package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.Cell.{Cell, _}
import ru.onehalf.japancrossword.model.line.{Line, LineImpl}

/**
  * Represents a strategy to solve one [[Line]] of the crossword.
  *
  * @since 24.05.13 23:09
  * @author OneHalf
  */
trait LineSolver {

  val SEPARATOR: List[Cell] = List(CLEARED)

  def fillLine(currentData: Line): Line

  /**
   * Схлопывание строки в одну. Если значение в списках не совпадают,
   * то в результате по соответсвующему индексу будет NOT_KNOWN.
   * В случае совпаднения значений, содержимое попадет в результирующий список
   * @param line1 Строка 1
   * @param line2 Строка 2
   * @return Результат объединения
   */
  protected def reduceLines(line1: Line, line2: Line): Line = {
    assert(line1.metadata == line2.metadata, s"lines: ${line1.size}, ${line2.size}")
    assert(line1.size == line2.size, s"lines: ${line1.size}, ${line2.size}")
    val lineLength = line1.size

    val result = Array.fill[Cell](lineLength)(NOT_KNOWN)

    // Сохряняем в результат только совпадающие данные
    0 until lineLength filter (i => line1(i) == line2(i)) foreach(i => result(i) = line2(i))
    new LineImpl(line1.metadata, result)
  }

  /**
   * Проверка, не противоречит ли предлагаемое значение текущим данным
   * @param currentData Текущие данные в модели
   * @param supposeLine Предлагаемая линия (Может содержать NOT_KNOWN)
   * @return true, если вариант приемлим
   */
  def compatibleToCurrentData(currentData: Line, supposeLine: Line): Boolean = {

    def cellIsCompatible(i: Int): Boolean = {
      currentData(i) == supposeLine(i) || currentData(i) == NOT_KNOWN || supposeLine(i) == NOT_KNOWN
    }

    assert(currentData.size == supposeLine.size)
    supposeLine.toList.indices forall cellIsCompatible
  }

  override def toString: String = getClass.getSimpleName
}
