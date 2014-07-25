
package wok.csv


class JointCharSequence(a: CharSequence, b: CharSequence) extends CharSequence {
  lazy val length = a.length + b.length

  def charAt(i: Int) = {
    if (i < 0 || i >= length) {
      throw new IndexOutOfBoundsException
    }
    if (i < a.length) {
      a.charAt(i)
    } else {
      b.charAt(i-a.length)
    }
  }

  def subSequence(s: Int, e: Int) = {
    if (s < 0 || e < 0 || s > e || e > length) {
      throw new IndexOutOfBoundsException
    }
    if (s < a.length) {
      if (e <= a.length) {
        a.subSequence(s, e)
      } else {
        val _e = e-a.length
        val _a = if (s == 0) a else a.subSequence(s, a.length)
        val _b = if (_e == b.length) b else b.subSequence(0, _e)
        new JointCharSequence(_a, _b)
      }
    } else {
      b.subSequence(s-a.length, e-a.length)
    }
  }

  override def toString: String = a.toString + b.toString

  override def equals(other: Any) = other match {
    case that: JointCharSequence if that.isInstanceOf[JointCharSequence] => toString == that.toString
    case _ => false
  }
}