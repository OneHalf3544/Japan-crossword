package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel

/**
 * Окошко с кроссвордом
 * <p/>
 * <p/>
 * Created: 02.05.13 0:16
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordFrame(model: JapanCrosswordModel, CELL_SIZE: Int, FONT_SIZE: Int) extends JFrame("Японский кроссворд") {

  model addListener (() => SwingUtilities.invokeLater(new Runnable {
    def run() {
      repaint()
    }
  }))

  initializeComponents()

  def initializeComponents() {

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    setContentPane(contentPane(CELL_SIZE, FONT_SIZE))

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    pack()
  }


  def contentPane(CELL_SIZE: Int, FONT_SIZE: Int): JPanel = {
    val contentPane = new JPanel(new BorderLayout())
    contentPane.add(new ModelChoosePanel(model), BorderLayout.PAGE_START)
    contentPane.add(new JScrollPane(new JapanCrosswordPanel(model, CELL_SIZE, FONT_SIZE)), BorderLayout.CENTER)
    contentPane
  }
}
