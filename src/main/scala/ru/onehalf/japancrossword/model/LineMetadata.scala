package ru.onehalf.japancrossword.model

import java.awt.Color


/**
 * <p/>
 * <p/>
 * Created: 08.06.13 14:23
 * <p/>
 * @author OneHalf
 */
class LineMetadata(private val content: Array[(Int, Color)]) {

  def this(i: Int, c: Color) = this(Array((i, c)))

  def this(v: Int) = this(v, Color.BLACK)

  def this(v: (Int, Color)) = this(Array(v))

  def this(v: Array[Int]) = this(v.map((_, Color.BLACK)).toList.toArray)

  def nonEmpty: Boolean = content.nonEmpty

  def count(predicate: (Int) => Boolean) = content.map(_._1).count(predicate)

  def isEmpty = content.isEmpty

  def reverse: LineMetadata = new LineMetadata(content.reverse)

  def size = content.size

  def min = content.map(_._1).min

  def max = content.map(_._1).max

  def sum = content.map(_._1).sum

  def map[B](f: (Int) => B) = content.map(_._1).map(f)

  def head = content.head
  def tail = new LineMetadata(content.tail)
  def last = content.last
  def init = new LineMetadata(content.init)

  def drop(i: Int) = new LineMetadata(content.drop(i))

  def minLength: Int = sum + content.map(_._2).sliding(2).filter(v => v.size > 1 && v(0) == v(1)).size

  def splitByFirstChunk(length: Int) = {
    val s = content.splitAt(content.map(_._1).indexOf(length))
    (new LineMetadata(s._1), new LineMetadata(s._2))
  }

  def apply(i: Int): (Int, Color) = content(i)

  def toList = content.toList

  override def toString: String = content.map(_._1).mkString("[", ",", "]")

  def eq(other: LineMetadata): Boolean = {
    other.isInstanceOf[LineMetadata] &&
      this.content.corresponds(other.content)((a,b) => a == b)
  }
}
