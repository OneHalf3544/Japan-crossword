package ru.onehalf.japancrossword.view

import javax.swing.{AbstractAction, JButton, JPanel}
import java.awt.FlowLayout
import java.awt.event.ActionEvent
import ru.onehalf.japancrossword.solver.{VariantsEnumerationSolver, BorderSolver, FastPreSolver}
import ru.onehalf.japancrossword.model.JapanCrosswordModel
import concurrent.future
import concurrent.ExecutionContext.Implicits.global

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 21:05
 * <p/>
 * @author OneHalf
 */
class ModelChoosePanel(model: JapanCrosswordModel) extends JPanel(new FlowLayout()) {

  add(solveButton)
  //add(editButton)
  add(clearButton)

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
