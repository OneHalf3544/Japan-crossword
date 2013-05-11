package ru.onehalf.japancrossword.view

import java.awt.{Font, Color, Graphics, Dimension}
import java.awt.event.{MouseEvent, MouseListener}
import ru.onehalf.japancrossword.model.{JapanCrosswordModel, Cell}
import javax.swing.JPanel

/**
 * <p/>
 * <p/>
 * Created: 11.05.13 11:25
 * <p/>
 * @author OneHalf
 */
class GridPanel(model: JapanCrosswordModel, CELL_SIZE: Int) extends JPanel {

  val left = 0
  val top = 0
  val right = model.columnNumber * CELL_SIZE
  val bottom = model.rowNumber * CELL_SIZE

  setPreferredSize(new Dimension(right + 1, bottom + 1))

  def determineCellCoordinate(x: Int, y: Int) = (x / CELL_SIZE, y / CELL_SIZE)

  // todo Заполнение ячеек "протаскиванием"
  addMouseListener(new MouseListener {
    def mouseExited(e: MouseEvent) {}
    def mouseClicked(e: MouseEvent) {}
    def mouseEntered(e: MouseEvent) {}
    def mousePressed(e: MouseEvent) {}
    def mouseReleased(e: MouseEvent) {
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

    g.setColor(Color.WHITE)
    g.fillRect(left, top, model.columnNumber * CELL_SIZE,  model.rowNumber * CELL_SIZE)

    g.setColor(Color.BLACK)
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
