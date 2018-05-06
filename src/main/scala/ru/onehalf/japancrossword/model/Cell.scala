package ru.onehalf.japancrossword.model

/**
  * Represents a cell status.
  *
  * @since 06.05.13 23:36
  * @author OneHalf
  */
object Cell extends Enumeration {

  type Cell = Value
  val FILLED, CLEARED, NOT_KNOWN = Value
}
