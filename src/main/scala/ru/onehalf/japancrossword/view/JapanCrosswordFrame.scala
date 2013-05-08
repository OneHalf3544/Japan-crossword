package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import ru.onehalf.japancrossword.Solver
import javax.swing.event._
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

/**
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

    val solver = new Solver(model)
    val solveButton = new JButton(new AbstractAction("Решить") {
      def actionPerformed(e: ActionEvent) {
        for (x <- 1 to model.columnNumber) {
          solver.fillColumn(x-1)
        }
        for (y <- 1 to model.rowNumber) {
          solver.fillRows(y-1)
        }
      }
    })

    val controlPanel = new JPanel(new FlowLayout())
    controlPanel.add(solveButton)
    controlPanel.add(new JButton(new AbstractAction("Очистить") {
      def actionPerformed(e: ActionEvent) {
        model.clear()
      }
    }))

    val contentPane = new JPanel(new BorderLayout())
    contentPane.add(new JapanCrosswordPanel(model), BorderLayout.CENTER)
    contentPane.add(controlPanel, BorderLayout.PAGE_END)
    setContentPane(contentPane)

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    pack()
    setVisible(true)
  }
}
