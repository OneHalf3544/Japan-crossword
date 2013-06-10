package ru.onehalf.japancrossword.model

import java.awt.Color

/**
  * Represents a cell status.
  *
  * @since 06.05.13 23:36
  * @author OneHalf
  */
abstract class Cell(val isFilled: Boolean, mayBeCleared: Boolean) {
  def isCleared: Boolean
  def isNotKnown: Boolean
}

case class NotKnownCell(content: Set[Color], mayBeCleared: Boolean) extends Cell(false, mayBeCleared) {
  def isCleared = content.isEmpty
  def isNotKnown = content.nonEmpty
}

case class FilledCell(color: Color) extends Cell(true, false) {
  def isCleared = false
  def isNotKnown = false
  override def toString: String = "FilledCell#" + Integer.toHexString(color.getRGB)
}

object Cleared extends Cell(false, true) {
  def isCleared = true
  def isNotKnown = false
  override def toString = "Cleared"
}

object Cell {

  def hasCommonState(cell1: Cell, cell2: Cell): Boolean = {
    (cell1, cell2) match {
      case (Cleared, Cleared) => true

      case (NotKnownCell(_, true), Cleared) => true
      case (Cleared, NotKnownCell(_, true)) => true

      case (NotKnownCell(colors, _), FilledCell(color)) if (colors.contains(color)) => true
      case (FilledCell(color), NotKnownCell(colors, _)) if (colors.contains(color)) => true

      case (FilledCell(color1), FilledCell(color2)) if (color1 == color2) => true

      case (NotKnownCell(colors1, mayBeCleared1), NotKnownCell(colors2, mayBeCleared2))
        if ((mayBeCleared1 == mayBeCleared2) || colors1.intersect(colors2).nonEmpty ) => true

      case _ => false
    }
  }

  /**
   *
   * @param cell1
   * @param cell2
   * @return
   */
  def combine(cell1: Cell, cell2: Cell): Cell = {
    (cell1, cell2) match {
      case (Cleared, Cleared) => Cleared
      case (Cleared, FilledCell(color)) => new NotKnownCell(Set(color), true)
      case (FilledCell(color), Cleared) => new NotKnownCell(Set(color), true)

      case (NotKnownCell(colors, _), Cleared) => new NotKnownCell(colors, true)
      case (Cleared, NotKnownCell(colors, _)) => new NotKnownCell(colors, true)

      case (FilledCell(color1), FilledCell(color2)) if (color1 == color2) => FilledCell(color1)
      case (FilledCell(color1), FilledCell(color2)) => new NotKnownCell(Set(color1, color2), false)

      case (NotKnownCell(colors, mayBeCleared), FilledCell(color)) => new NotKnownCell(colors + color, mayBeCleared)
      case (FilledCell(color), NotKnownCell(colors, mayBeCleared)) => new NotKnownCell(colors + color, mayBeCleared)

      case (NotKnownCell(colors1, mayBeCleared1), NotKnownCell(colors2, mayBeCleared2)) =>
        NotKnownCell(colors1 ++ colors2, mayBeCleared1 && mayBeCleared2)
    }
  }

  def reduce(cell1: Cell, cell2: Cell): Cell = {
    (cell1, cell2) match {
      case (NotKnownCell(_, _), Cleared) => Cleared
      case (Cleared, NotKnownCell(_, true)|Cleared) => Cleared

      case (FilledCell(color), NotKnownCell(colors, _)) if colors.contains(color) => FilledCell(color)
      case (NotKnownCell(colors, _), FilledCell(color)) if colors.contains(color) => FilledCell(color)
      case (FilledCell(color1), FilledCell(color2)) if (color1 == color2) => FilledCell(color1)

      case (NotKnownCell(colors1, true), NotKnownCell(colors2, true)) if colors1.intersect(colors2).isEmpty => Cleared
      case (NotKnownCell(colors1, mayBeCleared1), NotKnownCell(colors2, mayBeCleared2)) =>
        NotKnownCell(colors1.intersect(colors2), mayBeCleared1 && mayBeCleared2)
    }
  }
}