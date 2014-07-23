
package wok.csv


case class Quote(M: Quote.Mode.Type=Quote.Mode.Min, Q: Option[Char] = Some('"'), E: Option[Char] = scala.None) {
  def All() = this.copy(M=Quote.Mode.All, Q=if (Q.isDefined) Q else Some('"'))
  def Min() = this.copy(M=Quote.Mode.Min, Q=if (Q.isDefined) Q else Some('"'))
  def None() = this.copy(M=Quote.Mode.None, Q=scala.None)
  def Q(c: Char): Quote = this.copy(Q=Some(c))
  def E(c: Char): Quote = this.copy(E=Some(c))
}

object Quote {
  def All() = new Quote().All()
  def Min() = new Quote().Min()
  def None() = new Quote().None()
  def Q(c: Char) = new Quote().Q(c)
  def E(c: Char) = new Quote().E(c)

  object Mode {
    trait Type
    case object All extends Type
    case object Min extends Type
    case object None extends Type
  }
}
