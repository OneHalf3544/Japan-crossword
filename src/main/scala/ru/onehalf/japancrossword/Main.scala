package ru.onehalf.japancrossword

import solver.Orientation
import view.JapanCrosswordFrame
import model.{Metadata, JapanCrosswordModel}
import javax.swing.JFrame

/**
 * <p/>
 * <p/>
 * Created: 30.11.12 0:20
 * <p/>
 * @author OneHalf
 */
object Main {

  def parseParams(param: String): Array[Int] = (param split " ").map (_.toInt)

  def parseLine(orientation: Orientation.Orientation, string: String) : Metadata = {
    new Metadata(orientation, (string split ",\\s+").map(parseParams(_)))
  }

  def main(args: Array[String]) {

/*
    val horizonLine = parseLine(
      "2, 4, 4 2 3, 1 3 1 2, 1 2 2 2, 2 2 2 5, 2 1 2 7, 2 4 5, 2 3, 3 1 2 3, " +
      "1 7 2, 1 4 1 2, 1 4 1 2, 1 7 2, 4 1 2 3, 2 3, 2 2 5 4, 1 2 2 7, 1 3 5, " +
      "1 1 2 2, 2 1 2 2, 4 1 3, 1 2, 4, 2")

    val verticalLine = parseLine (
      "4, 1 2 5, 1 2 2 2, 2 1 2 2 1 1, 1 2 2 1 3 1, " +
      "2 1 1 1 4, 2 6 2, 2 2, 2 1, 1 1 1 1, 1 4 1, 1 6 1, 1 6 1, 1 4 1, " +
      "3 1 1 1 1 3, 2 4 1 1 4 2, 1 3 4 3 1, 3 5 5 4, 25, 4 10 4")
*/

    val horizonLine = parseLine(Orientation.HORIZONTAL, "1 1, 8, 4 3, 3 1 3, 3 6, 3 1, 3 1, 4, 8, 1 1")
    val verticalLine = parseLine(Orientation.VERTICAL, "2, 4, 6, 3 3, 3 3, 8, 1 1 1, 1 1 1, 4 1, 4 1, 5 2")


/*
    val horizonLine = parseLine(
      "3 7, 5 2 4, 7 1 5 4, 3 3 1 1 2 1 2 4, 2 4 3 3 1 1 1 1 3, 5 11 1 2 2 3, 2 1 1 5 3 1 5 3, " +
      "2 2 1 3 1 1 1 3 5, 2 2 13 4, 2 4 1 3 2 4, 9 8, 3 1 1 1 1 1 2 8, 15 11, 3 1 3 1 1 2 8, " +
      "4 4 5 2, 2 3 1 3 1 5, 2 2 5 5 1 3, 2 1 2 5 2 4 3, 2 1 1 4 5 8, 5 6 3 1 1 2 1 2, 2 1 1 3 3 5 2 4, " +
      "4 4 1 1 1 1 1 4, 7 1 2 2 4, 1 3 2 1 4, 3 5")

    val verticalLine = parseLine ("1, 4 3 4, 6 3 6, 2 1 2 3 2 1 2, 7 2 1 2 7, 3 2 2 5 1 1 2 1, 3 1 1 3 1 1 3 1 3, " +
      "3 2 1 7 1 1 4, 3 2 3 2 2 2 3, 21, 6 1 1 1 6, 17, 1 2 1 1 1 1 2 1 1, 2 3 3 1 2 1, 4 3 1 3 4, " +
      "1 8 8 1, 1 1 2 1 5 4 1 1, 3 1 4 1 3 1 1 2, 1 1 1 1 7 2 2 1 1, 1 2 1 2 4 1 2 2 2, 1 1 1 15 1 1, 1 2 3 7 2 1 3, " +
      "2 4 2 3 2 2 1 2, 3 4 3 6 2, 9 3 3 4, 3 1 2 3 3 2, 3 2 1 1 3, 5 3, 3 2, 1 1")
*/


    val model = new JapanCrosswordModel(horizonLine, verticalLine)
    val frame = new JapanCrosswordFrame(model)
  }
}
