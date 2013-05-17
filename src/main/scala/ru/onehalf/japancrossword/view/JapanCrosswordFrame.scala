package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import java.awt.event.{ActionEvent, ActionListener}

/**
 * Окошко с кроссвордом
 * <p/>
 * <p/>
 * Created: 02.05.13 0:16
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordFrame(models: Array[JapanCrosswordModel], CELL_SIZE: Int, FONT_SIZE: Int) extends JFrame("Японский кроссворд") {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  var crosswordPanel: JComponent = new JapanCrosswordPanel(models(0), CELL_SIZE, FONT_SIZE)
  val modelChoosePanel = new ModelChoosePanel(models, new ActionListener(){
    def actionPerformed(e: ActionEvent) {
      //setCrosswordPanel()
    }
  })

  models(0) addListener (() => SwingUtilities.invokeLater(new Runnable {
    def run() {
      repaint()
    }
  }))

  initializeComponents()



  def initializeComponents() {
    setContentPane(contentPane())
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    pack()
  }
  def contentPane(): JPanel = {

    val contentPane = new JPanel(new BorderLayout())
    contentPane.add(modelChoosePanel, BorderLayout.PAGE_START)
    contentPane.add(new JScrollPane(crosswordPanel), BorderLayout.CENTER)
    contentPane
  }

  def setCrosswordPanel(contentPane: JPanel, newCrosswordPanel: JComponent) {
    contentPane.remove(crosswordPanel)
    crosswordPanel = newCrosswordPanel
    contentPane.add(new JScrollPane(newCrosswordPanel), BorderLayout.CENTER)
  }
}
