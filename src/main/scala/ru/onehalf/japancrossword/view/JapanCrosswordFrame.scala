package ru.onehalf.japancrossword.view

import java.awt._
import javax.swing._
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import java.awt.event.ActionEvent
import ru.onehalf.japancrossword.solver.{BorderSolver, FastPreSolver, VariantsEnumerationSolver}
import concurrent.{ExecutionContext, future}
import ExecutionContext.Implicits.global

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
    contentPane.add(new JScrollPane(new JapanCrosswordPanel(model, CELL_SIZE, FONT_SIZE)), BorderLayout.CENTER)
    contentPane.add(controlPanel, BorderLayout.PAGE_END)
    contentPane
  }

  def controlPanel: JPanel = {
    val controlPanel = new JPanel(new FlowLayout())
    controlPanel.add(solveButton)
    //controlPanel.add(editButton)
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
      future {
        println("action performed")
        new FastPreSolver(model).solve()
        new BorderSolver(model).solve()
        new VariantsEnumerationSolver(model).solve()
      }
    }
  })
}
