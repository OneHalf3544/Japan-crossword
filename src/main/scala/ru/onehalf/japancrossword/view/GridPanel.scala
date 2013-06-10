package ru.onehalf.japancrossword.view

import java.awt.{Color, Graphics, Dimension}
import java.awt.event.{MouseEvent, MouseListener}
import ru.onehalf.japancrossword.model._
import javax.swing.JPanel
import ru.onehalf.japancrossword.model.FilledCell

/**
 * Панель с сеткой кроссворда. Реагирует на клики изменением модели
 * <p/>
 * <p/>
 * Created: 11.05.13 11:25
 * <p/>
 * @author OneHalf
 */
class GridPanel(model: Model, CELL_SIZE: Int) extends JPanel {

  private val left = 0
  private val top = 0
  private val right = model.columnNumber * CELL_SIZE
  private val bottom = model.rowNumber * CELL_SIZE

  setPreferredSize(new Dimension(right + 1, bottom + 1))

  private def determineCellCoordinate(x: Int, y: Int) = (x / CELL_SIZE, y / CELL_SIZE)

  // todo Заполнение ячеек "протаскиванием"
  addMouseListener(new MouseListener {
    def mouseExited(e: MouseEvent) {}
    def mouseClicked(e: MouseEvent) {}
    def mouseEntered(e: MouseEvent) {}
    def mousePressed(e: MouseEvent) {}
    def mouseReleased(e: MouseEvent) {
      val coordinates = determineCellCoordinate(e.getX, e.getY)
      if (!coordinatesInsideOfModel(coordinates)) {
        return
      }

      val current = model.apply(coordinates._1, coordinates._2)
      model.update(coordinates._1, coordinates._2, (current, e.getButton) match {
        case (NotKnownCell(_, _), MouseEvent.BUTTON1) => new FilledCell(Color.BLACK)
        case (NotKnownCell(_, _), MouseEvent.BUTTON3) => Cleared
        case (_, _) => NotKnownCell(model.colors, true)
      })
    }

    def coordinatesInsideOfModel(coordinates: (Int, Int)): Boolean = {
      val xInside = coordinates._1 >= 0 && coordinates._1 <= model.columnNumber
      val yInside = coordinates._2 >= 0 && coordinates._2 <= model.rowNumber
      xInside && yInside
    }
  })

  override def paint(g: Graphics) {

    g.setColor(Color.WHITE)
    g.fillRect(left, top, model.columnNumber * CELL_SIZE,  model.rowNumber * CELL_SIZE)

    g.setColor(Color.BLACK)
    val drawVerticalLine = (x: Int) => g.drawLine(left + x * CELL_SIZE, top, left + x * CELL_SIZE, bottom)
    val drawHorizonLine = (y: Int) => g.drawLine(left, top + y * CELL_SIZE, right, top + y * CELL_SIZE)

    g.setColor(Color.LIGHT_GRAY)
    1 until model.columnNumber foreach drawVerticalLine
    1 until model.rowNumber foreach drawHorizonLine

    g.setColor(Color.BLACK)
    5.to(model.columnNumber - 1, 5) foreach drawVerticalLine
    5.to(model.rowNumber - 1, 5) foreach drawHorizonLine

    g.setColor(Color.BLACK)
    g.drawRect(left, top, CELL_SIZE * model.columnNumber, CELL_SIZE * model.rowNumber)

    g.setColor(Color.BLACK)
    g.setPaintMode()

    def drawCell(x: Int, y: Int) {
      model.apply(x, y) match {
        case FilledCell(_) => g.fillRect(left + x * CELL_SIZE, top + y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
        case Cleared => {
          val x1 = left + x * CELL_SIZE
          val y1 = top + y * CELL_SIZE
          g.drawLine(x1, y1, x1 + CELL_SIZE, y1 + CELL_SIZE)
          g.drawLine(x1 + CELL_SIZE, y1, x1, y1 + CELL_SIZE)
        }
      }
    }

    for (x <- 0 until model.columnNumber)
      for (y <- 0 until model.rowNumber)
        if (!model(x, y).isNotKnown)
          drawCell(x, y)
  }

}
