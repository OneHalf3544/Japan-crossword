package ru.onehalf.japancrossword.model

/**
 * <p/>
 * <p/>
 * Created: 06.05.13 23:36
 * <p/>
 * @author OneHalf
 */
object Cell extends Enumeration {

  type Cell = Value
  val FILLED, CLEARED, NOT_KNOWN = Value
}
