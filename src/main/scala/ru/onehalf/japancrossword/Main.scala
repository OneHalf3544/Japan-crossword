package ru.onehalf.japancrossword

import view.JapanCrosswordFrame
import model.JapanCrosswordModel
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

  def parseLine(string: String) : Array[Array[Int]] = (string split ",\\s+").map(parseParams(_))

  def main(args: Array[String]) {

    val horizonLine = parseLine(
      "2, 4, 4 2 3, 1 3 1 2, 1 2 2 2, 2 2 2 5, 2 1 2 7, 2 4 5, 2 3, 3 1 2 3, " +
      "1 7 2, 1 4 1 2, 1 4 1 2, 1 7 2, 4 1 2 3, 2 3, 2 2 5 4, 1 2 2 7, 1 3 5, " +
      "1 1 2 2, 2 1 2 2, 4 1 3, 1 2, 4, 2")

    val verticalLine = parseLine (
      "4, 1 2 5, 1 2 2 2, 2 1 2 2 1 1, 1 2 2 1 3 1, " +
      "2 1 1 1 4, 2 6 2, 2 2, 2 1, 1 1 1 1, 1 4 1, 1 6 1, 1 6 1, 1 4 1, " +
      "3 1 1 1 1 3, 2 4 1 1 4 2, 1 3 4 3 1, 3 5 5 4, 25, 4 10 4")

    val frame = new JapanCrosswordFrame(new JapanCrosswordModel(horizonLine, verticalLine))
  }
}
