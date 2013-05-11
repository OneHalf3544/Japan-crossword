package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import java.awt.event.ActionEvent
import ru.onehalf.japancrossword.solver.Solver

/**
 * Окошко с кроссвордом
 * <p/>
 * <p/>
 * Created: 02.05.13 0:16
 * <p/>
 * @author OneHalf
 */
class JapanCrosswordFrame(model: JapanCrosswordModel) extends JFrame("Японский кроссворд") {

  model addListener (() => repaint())
  initializeComponents()

  def initializeComponents() {

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    val CELL_SIZE = 25
    val FONT_SIZE = 16

    setContentPane(contentPane(CELL_SIZE, FONT_SIZE))

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    pack()
  }


  def contentPane(CELL_SIZE: Int, FONT_SIZE: Int): JPanel = {
    val contentPane = new JPanel(new BorderLayout())
    contentPane.add(new JScrollPane(new JapanCrosswordPanel(model, CELL_SIZE, FONT_SIZE)), BorderLayout.CENTER)
    contentPane.add(controlPanel, BorderLayout.PAGE_END)
    contentPane
  }

  def controlPanel: JPanel = {
    val controlPanel = new JPanel(new FlowLayout())
    controlPanel.add(solveButton)
    controlPanel.add(editButton)
    controlPanel.add(clearButton)
    controlPanel
  }

  def clearButton: JButton = new JButton(new AbstractAction("Очистить") {
    def actionPerformed(e: ActionEvent) {
      model.clear()
    }
  })

  def editButton: JButton = new JButton(new AbstractAction("Редактирование") {
      def actionPerformed(e: ActionEvent) {
        // todo
      }
    })

  def solveButton = new JButton(new AbstractAction("Решить") {
    def actionPerformed(e: ActionEvent) {
      new Solver(model).solve()
    }
  })


}
