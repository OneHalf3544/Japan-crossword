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
    // Джек воробей
    val horizonLine = parseLine(Orientation.HORIZONTAL,
      "4, 2 3, 3 2 2, 4 2 4 4 2, 4 4 2 17 5, 6 2 28, 22 12 7 3 4, 27 11 5 14, 16 2 3 38, " +
      "15 2 2 1 2 34 1, 6 14 5 4 2 8 15, 5 3 1 7 4 2 3 2 1 12 4, 13 3 1 10 15 1 2 5 3 3, " +
      "3 16 4 1 19 11, 2 5 1 8 1 2 3 21 3 9, 8 1 6 4 2 20 8, 4 1 4 7 19 1 9, 3 4 4 3 32, " +
      "2 1 2 1 4 25 2, 2 1 1 4 3 14 5 3, 2 1 1 5 12 4 5, 1 1 1 8 8 1 6, 1 1 1 3 1 2 2 2 17, " +
      "1 2 1 6 5 1 18, 1 2 6 3 2 3 2 1 2 3 8, 1 1 1 6 2 4 2 3 2 3 8, 1 1 2 7 3 5 2 3 2 12 5, " +
      "1 2 3 4 2 6 5 7 2 4, 2 2 3 2 3 1 1 2 4 1 1 4, 2 1 3 2 5 3 1 2 3 2 3, 2 1 2 3 2 1 3 4 6 9, " +
      "4 3 2 1 1 4 1 4 8 8 2, 8 1 2 1 1 3 1 24 1 3, 3 4 1 2 1 1 4 1 29, 3 2 2 1 1 2 1 4 33, 4 3 2 1 1 2 5 2 33, " +
      "4 1 3 2 3 3 4 4 25 10 1, 9 2 3 3 5 26 1 10 3, 4 9 2 1 6 22 2 1 9 5, 4 8 4 4 19 1 12 3 6, " +
      "13 4 4 11 3 2 2 13 7, 15 1 17 1 12 4 2 1 10, 14 1 32 4 15, 13 12 26 6 7, 44 5 3 2 7, " +
      "24 17 6 4 6, 17 3 1 6 14 9 2, 17 2 1 2 3 16 6 3, 20 1 3 3 22 4, 18 2 24, 18 16 3 1, " +
      "3 28 2 3, 25 1 3, 8 15 2 3, 3 11 1 2 2, 1 11 4 2, 2 9 4 2, 6 3 1, 6 2 2 1, 1 4 2 1 1, " +
      "4 1 1 1 3, 1 5 1 4, 2 2 6, 2 1 1 3 2, 4 1 4, 5 4 3, 15, 5 3, 5 2, 7 2")

    val verticalLine = parseLine(Orientation.VERTICAL,
      "22, 7 11, 5 10, 3 2 3 7, 4 1 2 6, 4 2 7 2 5, 5 2 4 6 2 4, 8 14, 3 3 11, 3 4 10, 3 2 8, " +
      "3 2 9, 3 4 16, 7 5 14, 7 11 11, 4 3 4 11 8, 5 4 13, 10 15, 5 7 2 5, 5 6 13, 3 1 7 4 6 , " +
      "3 2 6 18, 2 6 12, 2 3 22, 2 2 2 9 15, 3 4 3 4 2 5 9, 3 8 28, 2 8 8 5 1 12, 3 1 1 4 5 4 2 1 8 4, " +
      "3 2 5 2 5 2 2 10 4, 3 4 1 4 1 6 11 4, 3 3 4 1 2 7 8, 4 3 3 1 1 8 5, 6 2 2 10 3, 1 3 1 1 4 12 3, " +
      "1 2 3 1 1 2 10 5, 1 2 3 1 1 2 12 4, 2 1 1 2 1 2 5 13, 2 2 3 2 6 5 12, 1 3 3 6 6 5 6, " +
      "2 4 4 4 14 4, 2 4 5 3 4 6 7 4, 1 5 6 2 4 7 5 7, 2 5 7 2 2 5 13 7, " +
      "1 7 6 28 5, 1 7 11 2 9 8 2 1,  1 5 11 11 10 3 1, 7 8 2 2 23 1, 7 8 2 3 2 8 16, 8 9 1 2 1 8 16, " +
      "4 13 7 5 11, 4 3 10 8 5 14, 4 3 10 1 34, 2 2 3 13 16 16 2, 1 3 5 11 13 2 17 1, 9 10 1 2 7 23, " +
      "8 2 11 2 1 5 22 1, 9 2 11 2 6 5 10 3, 2 8 11 2 4 3 9 2 2 2, 2 4 4 6 2 1 4 4 11 2 5, " +
      "1 4 5 5 2 2 20 1 2 4, 1 9 1 3 3 3 16 4 2 4, 2 6 1 3 2 3 9 2 5 2 6, 2 6 1 2 1 2 1 8 9 2 5, " +
      "1 7 4 3 1 11 8 3 8, 7 3 10 15 11 4 4, 7 15 14 8 2 1 2, 7 8 6 9 6 1 2 3 1 2 1, 2 4 8 5 3 5 6 1 3 2 3 1, " +
      "2 4 6 2 4 2 5 6 2 3 2 3 1, 1 5 5 2 4 2 3 7 1 2 2 5, 6 5 2 4 1 4 2 4 1 2 2 1 1 1, 3 8 2 3 1 3 4 4 2 2 4 3, " +
      "3 4 5 2 2 3 3 2 1 7 1, 3 3 3 1 1 3 2 2 2 3 1 2 1")
*/

/*
    // Собачка в профиль
    val horizonLine = parseLine(Orientation.HORIZONTAL,
      "7, 4 2, 3 1 1, 1 1 1, 3 1 2, 1 1 1, 1 1 1, 1 2 1, 1 1 1, 3 1 1, 1 1 1 1 1, 1 1 1, 1 3 1 2, " +
      "1 4, 3 2, 11 2, 12 2, 11 2, 11 2, 9 2")

    val verticalLine = parseLine(Orientation.VERTICAL,
      "2 6, 3 1 5, 7 1 1 6, 2 1 1 2 1 5, 1 1 2 1 5, 1 2 5, 1 5, 1 5, 2 2 5, 1 2 2 5, 1 3 5, 2 5 3, 5 1, 8, 8")
*/

/*
    // Пиратский флаг
    val horizonLine = parseLine(Orientation.HORIZONTAL,
      "2, 10, 13, 5 1, 1 2, 12, 12, 12, 6 1 1, 7 1 2, 2 2 4, 1 1 1 4, 1 1 1 4, 1 1 2 3, " +
      "2 3 3, 7 1 2, 7 1 1, 12, 3 6, 2 2")

    val verticalLine = parseLine(Orientation.VERTICAL,
      "3 2, 2 5 4, 2 14, 2 6 4, 2 5 3, 2 5 1 1 3, 2 5 1 3, 2 3 2 5, 2 3 6 2, 2 5 2 2, 2 3 3 5, " +
      "2 4 6 3, 2 12 2, 2 1 7, 2")
*/

/*
    // Собачка
    val horizonLine = parseLine(Orientation.HORIZONTAL,
      "2, 4, 4 2 3, 1 3 1 2, 1 2 2 2, 2 2 2 5, 2 1 2 7, 2 4 5, 2 3, 3 1 2 3, " +
      "1 7 2, 1 4 1 2, 1 4 1 2, 1 7 2, 4 1 2 3, 2 3, 2 2 5 4, 1 2 2 7, 1 3 5, " +
      "1 1 2 2, 2 1 2 2, 4 1 3, 1 2, 4, 2")

    val verticalLine = parseLine(Orientation.VERTICAL,
      "4, 1 2 5, 1 2 2 2, 2 1 2 2 1 1, 1 2 2 1 3 1, " +
      "2 1 1 1 4, 2 6 2, 2 2, 2 1, 1 1 1 1, 1 4 1, 1 6 1, 1 6 1, 1 4 1, " +
      "3 1 1 1 1 3, 2 4 1 1 4 2, 1 3 4 3 1, 3 5 5 4, 25, 4 10 4")
*/

    // Инкогнито по венециански
    val horizonLine = parseLine(Orientation.HORIZONTAL,
      "5, 4 2 4, 4 1 5 3, 3 3 1 6 3, 2 2 1 2 7 3, 2 5 1 2 7 4, 1 9 3 14 3, 2 9 5 20 1, 3 8 7 8 11 2, " +
      "3 5 3 2 10 2 4, 5 6 4 2 5 6 1, 6 6 3 2 1 1 10, 5 6 1 3 2 10 1, 4 7 4 2 5 4, 3 9 2 2 4 5 2, " +
      "1 10 2 1 1 2 5 1, 1 12 2 2 1 4 4 1, 1 13 1 4 1 6 3 2, 1 13 1 4 1 8 3, 1 13 4 2 1 10 1, " +
      "1 14 3 1 3 11, 1 14 2 1 4 2 5 2, 2 14 1 9 1 1 4, 1 14 15 6, 1 17 9 7 5, 1 29 10 4, 1 15 9 13 4, " +
      "1 15 12 4 3, 1 17 11 6 2, 1 23 20 1, 2 16 5 15, 1 17 8 12, 1 13 7 9 5 2, 1 12 4 10 1 3, 2 11 7 7 6, " +
      "1 11 9 4 4, 1 9 11 5 4, 2 8 5 3 4, 1 7 7 5, 2 6 4 6, 2 6 2 9, 2 4 10, 2 2 8, 1 1 8, 3 7")

    val verticalLine = parseLine(Orientation.VERTICAL,
      "9, 2 4 5, 4 3 5 4, 2 1 2 13 4, 2 2 3 19 3, 1 4 4 22 2, 2 4 3 25 3, 1 5 4 27 1, 2 5 3 28 1, 3 7 30, " +
      "1 1 6 28, 1 8 28, 1 3 27, 2 27, 32, 29, 26, 8 2 5, 5 4 3 5, 1 5 2 1 2, 4 2 4 3 1 2, 5 8 3 2 2, " +
      "6 6 3 2 3, 1 1 1 3 3 2 3, 2 1 3 2 2, 2 2 4 4 2, 2 1 4 2 2, 3 2 3 2 3, 3 3 3 2 3, 2 4 3 2 2, " +
      "1 7 3 3 2, 4 9 3 3 3 2, 5 3 4 1 2 3 2, 5 5 1 2 2 1, 6 5 1 3 2 2, 5 1 2 4 2 3 2 2, " +
      "5 8 2 2 4 2 3, 4 9 5 4 2 4, 5 4 6 4 3 2 4, 6 4 6 5 3 3 3, 7 3 4 6 3 3 2 1, 7 3 1 4 10 4 2, " +
      "6 3 3 4 11 4 4, 7 4 3 3 3 5 2 5, 6 4 4 5 2 5 1 1 6, 1 3 1 2 4 3 2 5 1 7, 2 2 3 2 1 1 5 4 11, " +
      "7 1 1 1 1 1 6 2 12, 7 2 2 2 8 1 13, 6 3 8 9 13")

/*
    // Домик
    val horizonLine = parseLine(Orientation.HORIZONTAL, "1 1, 8, 4 3, 3 1 3, 3 6, 3 1, 3 1, 4, 8, 1 1")
    val verticalLine = parseLine(Orientation.VERTICAL, "2, 4, 6, 3 3, 3 3, 8, 1 1 1, 1 1 1, 4 1, 4 1, 5 2")
*/


/*    // Бабочка
    val horizonLine = parseLine(Orientation.HORIZONTAL,
      "3 7, 5 2 4, 7 1 5 4, 3 3 1 1 2 1 2 4, 2 4 3 3 1 1 1 1 3, 5 11 1 2 2 3, 2 1 1 5 3 1 5 3, " +
      "2 2 1 3 1 1 1 3 5, 2 2 13 4, 2 4 1 3 2 4, 9 8, 3 1 1 1 1 1 2 8, 15 11, 3 1 3 1 1 2 8, " +
      "4 4 5 2, 2 3 1 3 1 5, 2 2 5 5 1 3, 2 1 2 5 2 4 3, 2 1 1 4 5 8, 5 6 3 1 1 2 1 2, 2 1 1 3 3 5 2 4, " +
      "4 4 1 1 1 1 1 4, 7 1 2 2 4, 1 3 2 1 4, 3 5")

    val verticalLine = parseLine(Orientation.VERTICAL, "1, 4 3 4, 6 3 6, 2 1 2 3 2 1 2, 7 2 1 2 7, 3 2 2 5 1 1 2 1, 3 1 1 3 1 1 3 1 3, " +
      "3 2 1 7 1 1 4, 3 2 3 2 2 2 3, 21, 6 1 1 1 6, 17, 1 2 1 1 1 1 2 1 1, 2 3 3 1 2 1, 4 3 1 3 4, " +
      "1 8 8 1, 1 1 2 1 5 4 1 1, 3 1 4 1 3 1 1 2, 1 1 1 1 7 2 2 1 1, 1 2 1 2 4 1 2 2 2, 1 1 1 15 1 1, 1 2 3 7 2 1 3, " +
      "2 4 2 3 2 2 1 2, 3 4 3 6 2, 9 3 3 4, 3 1 2 3 3 2, 3 2 1 1 3, 5 3, 3 2, 1 1")*/


    new JapanCrosswordFrame(new JapanCrosswordModel(horizonLine, verticalLine)).setVisible(true)
  }
}
