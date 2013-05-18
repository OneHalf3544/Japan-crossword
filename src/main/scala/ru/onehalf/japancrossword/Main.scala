package ru.onehalf.japancrossword

import solver.Orientation
import view.JapanCrosswordFrame
import model.Metadata

/**
 * <p/>
 * <p/>
 * Created: 30.11.12 0:20
 * <p/>
 * @author OneHalf
 */
object Main {

  def parseParams(param: String): Array[Int] = (param split "\\s+").map (_.toInt)

  def parseLine(orientation: Orientation.Orientation, string: String) : Metadata = {
    new Metadata(orientation, (string split ",\\s+").map(parseParams(_)))
  }

  def main(args: Array[String]) {
    val CELL_SIZE = 15
    val FONT_SIZE = 9
    new JapanCrosswordFrame(CrosswordLoader.loadCrosswords("/crosswords.properties"), CELL_SIZE, FONT_SIZE).setVisible(true)
  }
}
