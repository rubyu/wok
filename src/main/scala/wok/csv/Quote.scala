
package wok.csv


case class Quote(M: Quote.Mode.Type=Quote.Mode.None, Q: Option[Char] = None, E: Option[Char] = None) {
  def All() = this.copy(M=Quote.Mode.All, Q=if (Q.isDefined) Q else Some('"'))
  def Min() = this.copy(M=Quote.Mode.Min, Q=if (Q.isDefined) Q else Some('"'))
  def None() = this.copy(M=Quote.Mode.None)
  def Q(c: Char): Quote = this.copy(Q=Some(c))
  def E(c: Char): Quote = this.copy(E=Some(c))
}

object Quote {
  object Mode {
    trait Type
    case object All extends Type
    case object Min extends Type
    case object None extends Type
  }
}
