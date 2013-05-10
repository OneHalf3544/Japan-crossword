package ru.onehalf.japancrossword.view

import javax.swing.{JLabel, JTable, JPanel}
import java.awt.BorderLayout

/**
 * <p/>
 * <p/>
 * Created: 09.05.13 16:22
 * <p/>
 * @author OneHalf
 */
class MetadataEditorPanel extends JPanel(new BorderLayout()) {

  add(new JLabel("Изменение метаданных"))
  add(new JTable(new MetadataTableModel))


}
