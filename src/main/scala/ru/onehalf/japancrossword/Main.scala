package ru.onehalf.japancrossword

import view.JapanCrosswordFrame

/**
 * <p/>
 * <p/>
 * Created: 30.11.12 0:20
 * <p/>
 * @author OneHalf
 */
object Main {

  def main(args: Array[String]) {
    val CELL_SIZE = 15
    val FONT_SIZE = 9
    val crosswords = CrosswordLoader.loadCrosswords("/crosswords.properties")

    new JapanCrosswordFrame(crosswords, CELL_SIZE, FONT_SIZE).setVisible(true)
  }
}
