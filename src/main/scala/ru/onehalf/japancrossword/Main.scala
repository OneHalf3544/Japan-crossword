package ru.onehalf.japancrossword

import view.JapanCrosswordFrame

/**
  * Starts the application
  *
  * @since 30.11.12 0:20
  * @author OneHalf
  */
object Main {

  def main(args: Array[String]) {
    val CELL_SIZE = 14
    val FONT_SIZE = 8
    val crosswords = CrosswordLoader.loadCrosswords("/crosswords.properties")

    new JapanCrosswordFrame(crosswords, CELL_SIZE, FONT_SIZE).setVisible(true)
  }
}
