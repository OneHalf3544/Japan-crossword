package ru.onehalf.japancrossword.view

import java.awt.{event, BorderLayout, Dimension}
import javax.swing.{AbstractAction, JButton, JPanel, JFrame}
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import ru.onehalf.japancrossword.Solver
import swing.event.ActionEvent
import java.awt.event.ActionListener

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

    val solver = new Solver(model)
    val solveButton = new JButton(new AbstractAction("Решить") {
      def actionPerformed(e: event.ActionEvent) {
        for (x <- 1 to model.columnNumber) {
          solver.fillColumn(x-1)
        }
        for (y <- 1 to model.rowNumber) {
          solver.fillRows(y-1)
        }
        repaint()
      }
    })

    val panel = new JPanel(new BorderLayout())
    panel.add(new JapanCrosswordPanel(model), BorderLayout.CENTER)
    panel.add(solveButton, BorderLayout.PAGE_END)

    setContentPane(panel)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    pack()
    setVisible(true)
  }
}
