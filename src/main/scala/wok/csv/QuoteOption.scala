
package wok.csv


trait QuoteMode
case object QuoteAll extends QuoteMode
case object QuoteMin extends QuoteMode
case object QuoteNone extends QuoteMode

case class QuoteOption(M: QuoteMode=QuoteNone, Q: Option[Char] = None, E: Option[Char] = None) {
  def All() = this.copy(M=QuoteAll, Q=if (Q.isDefined) Q else Some('"'))
  def Min() = this.copy(M=QuoteMin, Q=if (Q.isDefined) Q else Some('"'))
  def None() = this.copy(M=QuoteNone)
  def Q(c: Char): QuoteOption = this.copy(Q=Some(c))
  def E(c: Char): QuoteOption = this.copy(E=Some(c))
}