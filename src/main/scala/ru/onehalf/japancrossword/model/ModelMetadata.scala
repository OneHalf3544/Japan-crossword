package ru.onehalf.japancrossword.model

/**
  * A set of numbers. One nonogram contains two objects of this type.
  *
  * @author OneHalf
  * @since 11.05.13 12:56
  */
class ModelMetadata(val orientation: Orientation.Orientation,  content: Array[LineMetadata]) {

  /**
   * A count of lines for current object.
   */
  val size: Int = content.length

  val maxPartsCount: Int = content.map(_.length).max

  /**
    * Get data for n-th line.
    *
    * @param n line index
    * @return Digits for requested line
    */
  def apply(n: Int) = content(n)
}
