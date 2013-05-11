package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import javax.swing.event._
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import ru.onehalf.japancrossword.solver.Solver

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

    val solveButton = new JButton(new AbstractAction("Решить") {
      def actionPerformed(e: ActionEvent) {
        new Solver(model).solve()
      }
    })

    val controlPanel = new JPanel(new FlowLayout())
    controlPanel.add(solveButton)
    controlPanel.add(new JButton(new AbstractAction("Редактирование") {
      def actionPerformed(e: ActionEvent) {
        val dialog = new JDialog(JapanCrosswordFrame.this, "Редактировать данные кроссворда", true)
        dialog.setContentPane(new MetadataEditorPanel)
        dialog.setVisible(true)
      }
    }))
    controlPanel.add(new JButton(new AbstractAction("Очистить") {
      def actionPerformed(e: ActionEvent) {
        model.clear()
      }
      }))

    val CELL_SIZE = 25
    val FONT_SIZE = 16

    val contentPane = new JPanel(new BorderLayout())
    contentPane.add(new JScrollPane(new JapanCrosswordPanel(model, CELL_SIZE, FONT_SIZE)), BorderLayout.CENTER)
    contentPane.add(controlPanel, BorderLayout.PAGE_END)
    setContentPane(contentPane)

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    pack()
    setVisible(true)
  }
}
