
package wok.csv


/*
 * https://issues.scala-lang.org/browse/SI-7710
 */
class FastCharSequence(chars: Array[Char], val sb: Int, val eb: Int) extends CharSequence {
  def this(chars: Array[Char]) = this(chars, 0, chars.length)

  lazy val length = eb - sb

  def charAt(i: Int): Char = {
    if (i < 0 || i >= length) {
      throw new IndexOutOfBoundsException
    }
    chars(i + sb)
  }

  def subSequence(s: Int, e: Int): CharSequence = {
    if (s < 0 || e < 0 || s > e || e > length) {
      throw new IndexOutOfBoundsException
    }
    new FastCharSequence(chars, sb + s, sb + e)
  }

  override def toString(): String = new String(chars, sb, length)
}