
package wok.core


class PrintableElement(elm: Seq[String]) extends Seq[String] {
  def length = elm.length
  def apply(idx: Int) = elm(idx)
  def iterator = elm.iterator
}
