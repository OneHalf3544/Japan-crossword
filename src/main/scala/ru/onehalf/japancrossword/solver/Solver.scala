package ru.onehalf.japancrossword.solver

import ru.onehalf.japancrossword.model.LineTrait
import ru.onehalf.japancrossword.model.Cell._

/**
 * <p/>
 * <p/>
 * Created: 12.05.13 23:24
 * <p/>
 * @author OneHalf
 */
abstract class Solver {


  /**
   * Заполнить линию (Меняем значение только если оно еще не оперделено в модели)
   * @param metadata Данные по ожидаемому заполнению линии (цифры с краев кроссворда)
   * @param currentData Текущие данные
   * @return Предполагаемый вариант линии. Может содержать NOT_KNOWN значения
   */
  def fillSubLine(metadata: Array[Int], currentData: LineTrait): List[Cell]



}
