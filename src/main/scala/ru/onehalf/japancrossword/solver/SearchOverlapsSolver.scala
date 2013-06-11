package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model._
import ru.onehalf.japancrossword.model.FilledCell
import scala.Some
import java.awt.Color

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
  def fillLine(metadata: LineMetadata, currentData: Line): List[Cell] = {
    assert(metadata.nonEmpty)

    //println("metadata: %s, line: %s".format(metadata, currentData))

    assert(currentData.size >= metadata.minLength, "wrong length: " + currentData.size)

    def numerateChunk(v: (Int, Color, Boolean), cell: Cell): (Int, Color, Boolean) = cell match {
      case FilledCell(color) if v._3 & v._2 == color => (v._1, color, true)
      case FilledCell(color) if !v._3 || v._2 != color => (v._1 + 1, color,  true)
      case Cleared => (v._1, null, false)
    }

    val line1 = fitFromLeft(metadata, currentData).get.scanLeft((0, Color.YELLOW, false))(numerateChunk).drop(1).map(v => if (v._3) v._1 else 0)
    val line2 = fitFromRight(metadata, currentData).get.scanLeft((0, Color.YELLOW, false))(numerateChunk).drop(1).map(v => if (v._3) v._1 else 0)

    (0 until currentData.size)
      .map(i => if (line1(i) == line2(i)) line1(i) else 0)
      .map(v => if (v == 0) new NotKnownCell(metadata.colors, true) else new FilledCell(metadata(v-1)._2)).toList
  }

  /**
   * Заполнение линии согласно переданным в функцию метаданным.
   * Все кусочки создаются максимально смещенными влево
   * @param metadata Метаданные для линии
   * @param currentData текущее содержимое линии
   * @return Линия, подходящих под указанные метаданные
   */
  def fitFromLeft(metadata: LineMetadata, currentData: Line): Option[List[Cell]] = {

    if (metadata.isEmpty) {
      // Нету больше метаданных? Значит остаток строки пустой
      if (currentData.exists(_.isFilled)) {
        return None
      }
      return Some(List.fill(currentData.size)(Cleared))
    }

    val chunkLength = metadata.head._1
    val chunk = List.fill[Cell](chunkLength)(new FilledCell(metadata.head._2))

    if (currentData.size == chunkLength) {
      // Оставшаяся длина совпадает в оставшимся куском
      return Some(chunk)
    }

    for ( i <- 0 to currentData.size - metadata.minLength) {

      // Заполнение отступом + заполненный участок
      var lineStart: List[Cell] = List.fill(i)(Cleared) ::: chunk

      // Параметр для повтороного вызова метода
      var newCurrentData = currentData.drop(lineStart.size)

      // Если какая-то часть строки еще остается, добавляем разделительную ячейку
      if (newCurrentData.nonEmpty() && metadata.size > 1 && metadata(0)._2 == metadata(1)._2) {
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

  def fitFromRight(metadata: LineMetadata, currentData: Line): Option[List[Cell]] = {
    Some(fitFromLeft(metadata.reverse, currentData.reverse()).get.reverse)
  }

}
