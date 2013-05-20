package ru.onehalf.japancrossword.view

import javax.swing.{JComboBox, AbstractAction, JButton, JPanel}
import java.awt.FlowLayout
import java.awt.event.{ItemListener, ActionEvent}
import ru.onehalf.japancrossword.solver.{SearchClearedCellSolver, VariantsEnumerationSolver, BorderSolver, FastPreSolver}
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
class ModelChoosePanel(models: Array[JapanCrosswordModel], modelChangeListener: ItemListener) extends JPanel(new FlowLayout()) {

  val modelsCombobox: JComboBox[JapanCrosswordModel] = {
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
      modelsCombobox.getSelectedItem.asInstanceOf[JapanCrosswordModel].clear()
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
        val model = modelsCombobox.getSelectedItem.asInstanceOf[JapanCrosswordModel]
        println("selected model: " + model)
        println("selected models: " + modelsCombobox.getSelectedObjects.mkString("[", ", ", "]"))
        new FastPreSolver(model).solve()
        new BorderSolver(model).solve()
        new SearchClearedCellSolver(model).solve()
        new VariantsEnumerationSolver(model).solve()
      }.onFailure({
        case e: Exception => e.printStackTrace()
      })
    }
  })
}
