package ru.onehalf.japancrossword.view

import javax.swing.JPanel
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import java.awt.{Font, Color, Dimension, Graphics}
import javax.swing.text.Style

/**
 * <p/>
 * <p/>
 * Created: 05.05.13 22:53
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordPanel(model: JapanCrosswordModel) extends JPanel {

  val CELL_SIZE = 30

  val maxHorizonOffset = toOffset(model.horizonLine)
  val maxVerticalOffset = toOffset(model.verticalLine)

  def toOffset(line: Array[Array[Int]]): Int = {
    (line.map(_.size).max + 1) * CELL_SIZE
  }

  val left = maxHorizonOffset
  val top = maxVerticalOffset
  val right = maxHorizonOffset + model.columnNumber * CELL_SIZE
  val bottom = maxVerticalOffset + model.rowNumber * CELL_SIZE

  setPreferredSize(new Dimension(right + CELL_SIZE, bottom + CELL_SIZE))


  override def paint(g: Graphics) {

    def drawMarks(line: Array[Array[Int]], xCoordinate: (Int, Int, Int) => Int, yCoordinate: (Int, Int, Int) => Int) {
      for (i <- 0 to line.size - 1) {
        val size = line(i).size
        for (j <- 0 to size - 1) {
          val x = left + xCoordinate(size, i, j) * CELL_SIZE
          val y = top + yCoordinate(size, i, j) * CELL_SIZE

          g.setColor(Color.BLACK)
          g.drawString(line(i)(j).toString, x + CELL_SIZE / 4, y + CELL_SIZE * 3 / 4)

          g.setColor(Color.LIGHT_GRAY)
          g.drawRect(x, y, CELL_SIZE, CELL_SIZE)
        }
      }
    }

    g.setColor(Color.BLACK)
    g.setFont(new Font("Courier New", Font.BOLD, 16))

    drawMarks(model.horizonLine, (s, i, j) => i, (s, i, j) => j - s)
    drawMarks(model.verticalLine, (s, i, j) => j - s, (s, i, j) => i)

    g.setColor(Color.BLACK)
    g.drawRect(left, top, CELL_SIZE * model.columnNumber, CELL_SIZE * model.rowNumber)

    g.setColor(Color.LIGHT_GRAY)
    1 to (model.columnNumber - 1) foreach (x => g.drawLine(left + x * CELL_SIZE, top, left + x * CELL_SIZE, bottom))
    1 to (model.rowNumber - 1) foreach (y => g.drawLine(left, top + y * CELL_SIZE, right, top + y * CELL_SIZE))

    g.setColor(Color.BLACK)
    g.setPaintMode()
    for (x <- 0 to model.columnNumber - 1)
      for (y <- 0 to model.rowNumber - 1)
        if (model.board(x)(y))
          g.fillRect(left + x * CELL_SIZE, top + y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
  }
}
