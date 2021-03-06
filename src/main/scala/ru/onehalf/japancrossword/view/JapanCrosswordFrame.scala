package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import java.awt.event.{ItemEvent, ItemListener}

/**
  * A main application window.
  *
  * @since 02.05.13 0:16
  * @author OneHalf
  */
class JapanCrosswordFrame(models: Array[JapanCrosswordModel], CELL_SIZE: Int, FONT_SIZE: Int) extends JFrame("Японский кроссворд") {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  private val repaintListener = () => SwingUtilities.invokeLater(() =>  repaint())

  private var crosswordPanel: JComponent = new JScrollPane(new JapanCrosswordPanel(models(0), CELL_SIZE, FONT_SIZE))

  /**
   * Панелька с кнопками и комбобоксом
   */
  private val modelChoosePanel = new ControlPanel(models, new ItemListener(){
    def itemStateChanged(e: ItemEvent) {
      val model = e.getItem.asInstanceOf[JapanCrosswordModel]
      e.getStateChange match {
        case ItemEvent.SELECTED => {
          model.addListener(repaintListener)
          setCrosswordPanel(new JScrollPane(new JapanCrosswordPanel(model, CELL_SIZE, FONT_SIZE)))
        }
        case ItemEvent.DESELECTED => model.removeListener(repaintListener)
      }
    }
  })

  // Load first model at application start
  models(0) addListener repaintListener

  setContentPane(contentPane())
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  pack()

  def contentPane(): JPanel = {
    val contentPane = new JPanel(new BorderLayout())
    contentPane.add(modelChoosePanel, BorderLayout.PAGE_START)
    contentPane.add(crosswordPanel, BorderLayout.CENTER)
    contentPane
  }

  def setCrosswordPanel(newCrosswordPanel: JComponent) {
    getContentPane.remove(crosswordPanel)
    crosswordPanel = newCrosswordPanel
    getContentPane.add(newCrosswordPanel, BorderLayout.CENTER)
    validate()
  }
}
