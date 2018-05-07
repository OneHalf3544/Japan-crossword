package ru.onehalf.japancrossword.model.line

import org.scalatest.FlatSpec
import ru.onehalf.japancrossword.CrosswordLoader.parseLine
import ru.onehalf.japancrossword.model.{Cell, JapanCrosswordModel, Orientation}

class LineOfModelTest extends FlatSpec {

  "the 'dropClearedFromEnds' methods" should "delete cleared cells from the ends of line" in {
    val metadata = parseLine(Orientation.VERTICAL, "2 2")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 1, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)
    line(0) = Cell.CLEARED
    line(8) = Cell.CLEARED
    line(9) = Cell.CLEARED

    val result = line.dropClearedFromEnds()

    assert(result === new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model, 1, 7))
  }

  "the 'dropRight' method" should "delete last elements from line and metadata" in {
    val metadata = parseLine(Orientation.VERTICAL, "1 2")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 0, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    val result = line.dropRight(1, 3)

    assert(result === new LineOfModelImpl(new LineMetadata(1), 0, Orientation.HORIZONTAL, model, 0, 7))
  }

  "the 'dropLeft' method" should "delete last elements from line and metadata" in {
    val metadata = parseLine(Orientation.VERTICAL, "1 2")
    val model = new JapanCrosswordModel("test",
      parseLine(Orientation.HORIZONTAL, "0, 1, 0, 0, 1, 1, 0, 0, 0, 0"),  // 10 cells
      metadata)

    val line = new LineOfModelImpl(metadata(0), 0, Orientation.HORIZONTAL, model)

    val result = line.dropLeft(1, 3)

    assert(result === new LineOfModelImpl(new LineMetadata(2), 0, Orientation.HORIZONTAL, model, 2, 7))
  }

}
