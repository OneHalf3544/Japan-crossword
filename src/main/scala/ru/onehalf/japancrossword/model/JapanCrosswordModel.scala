package ru.onehalf.japancrossword.model

/**
 * <p/>
 * <p/>
 * Created: 05.05.13 22:54
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordModel(val horizonLine : Array[Array[Int]], val verticalLine : Array[Array[Int]]) {

  val columnNumber = horizonLine.size
  val rowNumber = verticalLine.size

  val board: Array[Array[Boolean]] = Array.ofDim[Boolean](columnNumber, rowNumber)
}
