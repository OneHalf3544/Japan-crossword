package ru.onehalf.japancrossword.model

import scala.Array

/**
 * Сигнатуры рядов.
 * <p/>
 * <p/>
 * Created: 11.05.13 12:56
 * <p/>
 * @author OneHalf
 */
class Metadata(val orientation: Orientation.Orientation,  content: Array[Array[Int]]) {

  /**
   * Число рядов, описываемых данным классом
   */
  val size: Int = content.size

  val maxPartsCount: Int = content.map(_.size).max

  def apply(i: Int) = content(i)
}
