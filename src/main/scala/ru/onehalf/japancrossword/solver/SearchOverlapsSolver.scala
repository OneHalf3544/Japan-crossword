package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.Line
import ru.onehalf.japancrossword.model.Cell._
import ru.onehalf.japancrossword.model.Cell.Cell

/**
  * Searches overlaps of the left and right positions to find filled cells.
  *
  * @since 12.05.13 22:51
  * @author OneHalf
  */
object SearchOverlapsSolver extends LineSolver {

  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   */
  def fillLine(metadata: Array[Int], currentData: Line): List[Cell] = {
    assert(metadata.nonEmpty)

    //println("metadata: %s, line: %s".format(metadata.mkString("[", ",", "]"), currentData))

    val length = metadata.sum + metadata.length - 1
    assert(currentData.size >= length, "wrong length: " + length)

    def numerateChunk(v: (Int, Boolean), cell: Cell) = cell match {
      case FILLED if v._2 => (v._1, true)
      case FILLED if !v._2 => (v._1 + 1, true)
      case CLEARED | NOT_KNOWN => (v._1, false)
    }

    val line1 = fitFromLeft(metadata, currentData).get.scanLeft((0, false))(numerateChunk).drop(1).map(v => if (v._2) v._1 else 0)
    val line2 = fitFromRight(metadata, currentData).get.scanLeft((0, false))(numerateChunk).drop(1).map(v => if (v._2) v._1 else 0)

    (0 until currentData.size)
      .map(i => if (line1(i) == line2(i)) line1(i) else 0)
      .map(v => if (v == 0) NOT_KNOWN else FILLED).toList
  }

  /**
   * Заполнение линии согласно переданным в функцию метаданным.
   * Все кусочки создаются максимально смещенными влево
   * @param metadata Метаданные для линии
   * @param currentData текущее содержимое линии
   * @return Линия, подходящих под указанные метаданные
   */
  def fitFromLeft(metadata: Array[Int], currentData: Line): Option[List[Cell]] = {

    if (metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      //assert(currentData.forall(_ != FILLED))
      return Some(List.fill(currentData.size)(CLEARED))
    }

    val chunkLength = metadata.head
    val chunk = List.fill[Cell](chunkLength)(FILLED)

    if (currentData.size == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return Some(chunk)
    }

    for ( i <- 0 to currentData.size - chunkLength - metadata.tail.map(_ + 1).sum) {

      // Заполнение отступом + заполненный участок
      var lineStart: List[Cell] = List.fill(i)(CLEARED) ::: chunk

      // Параметр для повтороного вызова метода
      var newCurrentData = currentData.drop(lineStart.size)

      // Если какая-то часть строки еще остается, добавляем разделительную ячейку
      if (newCurrentData.nonEmpty()) {
        lineStart = lineStart ::: SEPARATOR
        newCurrentData = newCurrentData.drop(1)
      }

      if (compatibleToCurrentData(currentData.toList.take(lineStart.size), lineStart)) {
          fitFromLeft(metadata.tail, newCurrentData) match {
            case Some(x) => return Some(lineStart ::: x)
            case None => ;
          }
        }

      // Продолжаем искать варианты
    }

    None
  }

  def fitFromRight(metadata: Array[Int], currentData: Line): Option[List[Cell]] = {
    Some(fitFromLeft(metadata.reverse, currentData.reverse()).get.reverse)
  }

}