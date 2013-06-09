package ru.onehalf.japancrossword.model

import java.awt.Color

/**
  * Represents a cell status.
  *
  * @since 06.05.13 23:36
  * @author OneHalf
  */
abstract class Cell(val isFilled: Boolean) {
  def isCleared: Boolean
  def isNotKnown: Boolean
}

case class NotKnownCell(content: Set[Color]) extends Cell(false) {
  def isCleared = content.isEmpty
  def isNotKnown = content.nonEmpty
}

case class FilledCell(color: Color) extends Cell(true) {
  def isCleared = false
  def isNotKnown = false
}

object Cleared extends Cell(false) {
  def isCleared = true
  def isNotKnown = false
}
