package ru.onehalf.japancrossword.view

import javax.swing.{JComboBox, AbstractAction, JButton, JPanel}
import java.awt.FlowLayout
import java.awt.event.{ItemListener, ActionEvent}
import ru.onehalf.japancrossword.model.Model
import concurrent.future
import concurrent.ExecutionContext.Implicits.global
import ru.onehalf.japancrossword.solver.ModelSolver

/**
 * <p/>
 * <p/>
 * Created: 15.05.13 21:05
 * <p/>
 * @author OneHalf
 */
class ControlPanel(models: Array[Model], modelChangeListener: ItemListener) extends JPanel(new FlowLayout()) {

  val modelsCombobox: JComboBox[Model] = {
    val comboBox = new JComboBox(models)
    comboBox.addItemListener(modelChangeListener)
    comboBox
  }

  add(modelsCombobox)
  add(solveButton)
  add(clearButton)
  //add(editButton)


  def clearButton: JButton = new JButton(new AbstractAction("Очистить") {
    def actionPerformed(e: ActionEvent) {
      modelsCombobox.getSelectedItem.asInstanceOf[Model].clear()
    }
  })

  def editButton: JButton = new JButton(new AbstractAction("Редактирование") {
    def actionPerformed(e: ActionEvent) {
      // todo
    }
  })

  def solveButton = new JButton(new AbstractAction("Решить") {
    def actionPerformed(e: ActionEvent) {
      val model = modelsCombobox.getSelectedItem.asInstanceOf[Model]
      future {
        new ModelSolver(model).solve()
      }.onFailure({
        case e: Exception => e.printStackTrace()
      })
    }
  })
}
