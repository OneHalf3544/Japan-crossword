package ru.onehalf.japancrossword.view

import javax.swing.table.{DefaultTableModel, TableModel}

/**
 * <p/>
 * <p/>
 * Created: 09.05.13 16:34
 * <p/>
 * @author OneHalf
 */
class MetadataTableModel(row: Int, column: Int) extends DefaultTableModel(row, column) {

  def this() = this(5, 15)
}
