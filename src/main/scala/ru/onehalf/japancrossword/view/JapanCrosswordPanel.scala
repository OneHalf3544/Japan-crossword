package ru.onehalf.japancrossword.view

import javax.swing.JPanel
import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel}
import java.awt.{Font, Color, Dimension, Graphics}
import java.awt.event.{MouseEvent, MouseListener}

/**
 * <p/>
 * <p/>
 * Created: 05.05.13 22:53
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordPanel(model: JapanCrosswordModel) extends JPanel {

  val CELL_SIZE = 25
  val FONT_SIZE = 16

  val maxHorizonOffset = toOffset(model.verticalLine)
  val maxVerticalOffset = toOffset(model.horizonLine)

  def toOffset(line: Array[Array[Int]]): Int = (line.map(_.size).max + 1) * CELL_SIZE

  val left = maxHorizonOffset
  val top = maxVerticalOffset
  val right = maxHorizonOffset + model.columnNumber * CELL_SIZE
  val bottom = maxVerticalOffset + model.rowNumber * CELL_SIZE

  setPreferredSize(new Dimension(right + CELL_SIZE, bottom + CELL_SIZE))

  def determineCellCoordinate(x: Int, y: Int) = ((x - left) / CELL_SIZE, (y - top) / CELL_SIZE)

  addMouseListener(new MouseListener {
    def mouseExited(e: MouseEvent) {}
    def mouseClicked(e: MouseEvent) {}
    def mouseEntered(e: MouseEvent) {}
    def mousePressed(e: MouseEvent) {}
    def mouseReleased(e: MouseEvent) {
      if (e.getX < left || e.getX > right || e.getY < top || e.getY > bottom) {
        return
      }

      val coordinates = determineCellCoordinate(e.getX, e.getY)
      val current = model.getCell(coordinates._1, coordinates._2)

      model.setCell(coordinates._1, coordinates._2, (current, e.getButton) match {
        case (Cell.NOT_KNOWN, MouseEvent.BUTTON1) => Cell.FILLED
        case (Cell.NOT_KNOWN, MouseEvent.BUTTON3) => Cell.CLEARED
        case (_, _) => Cell.NOT_KNOWN
      })
    }
  })

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

    g.setColor(Color.WHITE)
    g.fillRect(left, top, model.columnNumber * CELL_SIZE,  model.rowNumber * CELL_SIZE)

    g.setColor(Color.BLACK)
    g.setFont(new Font("Courier New", Font.BOLD, FONT_SIZE))

    drawMarks(model.horizonLine, (s, i, j) => i, (s, i, j) => j - s)
    drawMarks(model.verticalLine, (s, i, j) => j - s, (s, i, j) => i)

    val drawVerticalLine = (x: Int) => g.drawLine(left + x * CELL_SIZE, top, left + x * CELL_SIZE, bottom)
    val drawHorizonLine = (y: Int) => g.drawLine(left, top + y * CELL_SIZE, right, top + y * CELL_SIZE)

    g.setColor(Color.LIGHT_GRAY)
    1 to (model.columnNumber - 1) foreach drawVerticalLine
    1 to (model.rowNumber - 1) foreach drawHorizonLine

    g.setColor(Color.BLACK)
    5.to(model.columnNumber - 1, 5) foreach drawVerticalLine
    5.to(model.rowNumber - 1, 5) foreach drawHorizonLine

    g.setColor(Color.BLACK)
    g.drawRect(left, top, CELL_SIZE * model.columnNumber, CELL_SIZE * model.rowNumber)

    g.setColor(Color.BLACK)
    g.setPaintMode()

    def drawCell(x: Int, y: Int) {
      model.getCell(x, y) match {
        case Cell.FILLED => g.fillRect(left + x * CELL_SIZE, top + y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
        case Cell.CLEARED => {
          val x1 = left + x * CELL_SIZE
          val y1 = top + y * CELL_SIZE
          g.drawLine(x1, y1, x1 + CELL_SIZE, y1 + CELL_SIZE)
          g.drawLine(x1 + CELL_SIZE, y1, x1, y1 + CELL_SIZE)
        }
      }
    }

    for (x <- 0 to model.columnNumber - 1)
      for (y <- 0 to model.rowNumber - 1)
        if (model.getCell(x, y) != Cell.NOT_KNOWN)
          drawCell(x, y)
  }

}
