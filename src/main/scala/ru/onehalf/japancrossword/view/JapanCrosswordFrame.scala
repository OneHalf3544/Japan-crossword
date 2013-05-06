package ru.onehalf.japancrossword.view

import java.awt.Dimension
import javax.swing.{JPanel, JFrame}
import ru.onehalf.japancrossword.model.JapanCrosswordModel

/**
 * <p/>
 * <p/>
 * Created: 02.05.13 0:16
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordFrame(model: JapanCrosswordModel) extends JFrame("Японский кроссворд") {

  initializeComponents()

  def initializeComponents() {

    setContentPane(new JapanCrosswordPanel(model))

    pack()

    setVisible(true)
  }
}
