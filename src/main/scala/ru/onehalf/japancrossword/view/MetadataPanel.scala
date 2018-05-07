package ru.onehalf.japancrossword.view

import java.awt.{Font, Color, Graphics, Dimension}
import ru.onehalf.japancrossword.model.{Orientation, Metadata}
import javax.swing.JPanel

/**
  * A swing element for representing an area with digits (an initial conditions of crossword).
  *
  * @since 11.05.13 11:25
  * @author OneHalf
  */
class MetadataPanel(val CELL_SIZE: Int, val FONT_SIZE: Int, metadata: Metadata) extends JPanel {

  setPreferredSize(dimension)

  def dimension: Dimension = {
    def createDimension(width: Int, height: Int): Dimension = {
      new Dimension(width * CELL_SIZE + 1, height * CELL_SIZE + 1)
    }
    metadata.orientation match {
      case Orientation.HORIZONTAL =>
        createDimension(metadata.size, metadata.maxPartsCount)

      case Orientation.VERTICAL =>
        createDimension(metadata.maxPartsCount, metadata.size)
    }
  }

  override def paint(g: Graphics) {
    g.setColor(Color.BLACK)
    g.setFont(new Font("Courier New", Font.BOLD, FONT_SIZE))

    if (metadata.orientation == Orientation.HORIZONTAL)
      drawMarks(g, (s, i, j) => i, (s, i,  j) => metadata.maxPartsCount + j - s)
    else
      drawMarks(g, (s, i, j) => metadata.maxPartsCount + j - s, (s, i, j) => i)
  }

  def drawMarks(g: Graphics, xCoordinate: (Int, Int, Int) => Int, yCoordinate: (Int, Int, Int) => Int) {
    for (lineIndex <- 0 until metadata.size) {
      val size = metadata(lineIndex).size
      for (j <- 0 until size) {
        val x = xCoordinate(size, lineIndex, j) * CELL_SIZE
        val y = yCoordinate(size, lineIndex, j) * CELL_SIZE

        g.setColor(Color.BLACK)
        g.drawString(metadata(lineIndex)(j).toString, x + CELL_SIZE / 4, y + CELL_SIZE * 3 / 4)

        g.setColor(Color.LIGHT_GRAY)
        g.drawRect(x, y, CELL_SIZE, CELL_SIZE)
      }
    }
  }

}
