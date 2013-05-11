package ru.onehalf.japancrossword.view

import javax.swing.JPanel
import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel}
import java.awt._
import java.awt.event.{MouseEvent, MouseListener}

/**
 * Панелька с кроссводром (цифры + сетка)
 * <p/>
 * <p/>
 * Created: 05.05.13 22:53
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordPanel(model: JapanCrosswordModel,  CELL_SIZE: Int, FONT_SIZE: Int) extends JPanel(new GridBagLayout()) {

  initializeComponents()

  def initializeComponents() {

    add(new JPanel(), constraints(0, 0))
    add(new MetadataPanel(CELL_SIZE, FONT_SIZE, model.horizonLine), constraints(1, 0))
    add(new MetadataPanel(CELL_SIZE, FONT_SIZE, model.verticalLine), constraints(0, 1))
    add(new GridPanel(model, CELL_SIZE), constraints(1, 1))

    setBorder(javax.swing.BorderFactory.createEmptyBorder(CELL_SIZE, CELL_SIZE, CELL_SIZE, CELL_SIZE))
  }

  def constraints(x: Int, y: Int): GridBagConstraints = {
    val result = new GridBagConstraints()
    result.gridx = x
    result.gridy = y
    result
  }

}
