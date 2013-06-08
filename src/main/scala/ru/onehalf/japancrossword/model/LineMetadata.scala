package ru.onehalf.japancrossword.model


/**
 * <p/>
 * <p/>
 * Created: 08.06.13 14:23
 * <p/>
 * @author OneHalf
 */
class LineMetadata(private val content: Array[Int]) {

  def nonEmpty: Boolean = content.nonEmpty

  def this(i: Int) = this(Array(i))

  def count(predicate: (Int) => Boolean) = content.count(predicate)

  def isEmpty = content.isEmpty

  def reverse: LineMetadata = new LineMetadata(content.reverse)

  def size = content.size

  def min = content.min

  def max = content.max

  def sum = content.sum

  def map[B](f: (Int) => B) = content.map(f)

  def head = content.head
  def tail = new LineMetadata(content.tail)
  def last = content.last
  def init = new LineMetadata(content.init)

  def drop(i: Int) = new LineMetadata(content.drop(i))

  def splitByFirstChunk(length: Int) = {
    val s = content.splitAt(content.indexOf(length))
    (new LineMetadata(s._1), new LineMetadata(s._2))
  }

  def apply(i: Int): Int = content(i)

  def toList = content.toList


  override def toString: String = content.mkString("[", ",", "]")

  def eq(other: LineMetadata): Boolean = {
    other.isInstanceOf[LineMetadata] &&
      this.content.corresponds(other.content)((a,b) => a == b)
  }
}
