package ru.onehalf.japancrossword.model.line

class LineMetadata(private val digits: Array[Int]) extends Traversable[Int] {

  def this(digits: Traversable[Int]) = this(digits.toArray)

  def this(digits: Int*) = this(digits.toTraversable)

  def :+(newDigit: Int) = new LineMetadata(digits :+ newDigit)

  def dropFromBegining(metadata: LineMetadata): LineMetadata = {
    assert(metadata.size <= size, s"the argument ($metadata) cannot be longer than current ($this) line")
    assert(metadata.digits sameElements digits.take(metadata.size))

    new LineMetadata(digits.drop(metadata.digits.length))
  }


  def indexOf(number: Int): Int = digits.indexOf(number)

  override def last: Int = digits.last

  override def tail: LineMetadata = new LineMetadata(digits.tail)

  def reverse(): LineMetadata = new LineMetadata(digits.reverse)

  override def size: Int = digits.length

  def minimalLineLength: Int = digits.sum + digits.length - 1

  override def toString: String = digits.mkString("[", ",", "]")

  def apply(index: Int): Int = digits(index)

  def canEqual(other: Any): Boolean = other.isInstanceOf[LineMetadata]

  override def equals(other: Any): Boolean = other match {
    case that: LineMetadata =>
      (that canEqual this) &&
        (digits sameElements that.digits)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(digits)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def foreach[U](f: Int => U): Unit = digits.foreach(f)

  def splitByFirstChunk(length: Int) = {
    val s = digits.splitAt(digits.map(_._1).indexOf(length))
    (new LineMetadata(s._1), new LineMetadata(s._2))
  }
}

object LineMetadata {

  def empty() = new LineMetadata(Array.emptyIntArray)

  def metadata(digits: Int*) = new LineMetadata(digits)
}
