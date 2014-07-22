
package wok.csv

import java.io.OutputStream
import wok.core.Helpers._
import scalax.io.Codec


class Writer {
  private var ofs = " "
  private var ors = "\n"
  private var ofq = Quote()
  private var ocd = Codec.default

  def OFS = ofs
  def ORS = ors
  def OFQ = ofq
  def OCD = ocd

  def OFS(s: String) = { ofs = s; update(); this }
  def ORS(s: String) = { ors = s; update(); this }
  def OFQ(q: Quote) = { ofq = q; update(); this }
  def OCD(c: Codec) = { ocd = c; this }

  private def _escape: String => String = {
    ofq match {
      //escapes e and q in a given string with e and quotes it with q
      case Quote(Quote.Mode.All, Some(q), Some(e)) => { _.escaped(e, q).quoted(q) }

      //quotes a given string with q
      case Quote(Quote.Mode.All, Some(q), None) => { _.escaped(q).quoted(q) }

      //escapes e and q in a given string with e and quotes it with q when it contains OFS or ORS
      case Quote(Quote.Mode.Min, Some(q), Some(e)) => {
        case s if s.contains(ofs) || s.contains(ors) => s.escaped(e, q).quoted(q)
        case s => s.escaped(e, q)
      }

      //escapes q in a given string with q and quotes it with q when it contains OFS or ORS or q
      case Quote(Quote.Mode.Min, Some(q), None) => {
        case s if s.contains(ofs) || s.contains(ors) || s.contains(q) => s.escaped(q).quoted(q)
        case s => s
      }

      //escapes OFS in a given string with e and throws an error when it contains ORS
      case Quote(Quote.Mode.None, _, Some(e)) => {
        case s if s.contains(ors) => throw new EncodingException(s"data must not contain '$ors'")
        case s => s.escaped(e, ofs)
      }

      //throws an error when a given string contains OFS or ORS
      case Quote(Quote.Mode.None, _, None) => {
        case s if s.contains(ofs) || s.contains(ors) => throw new EncodingException(s"data must not contain '$ofs' and '$ors'")
        case s => s
      }
    }
  }

  private var escape = _escape
  private def update(): Unit = escape = _escape

  def write(out: OutputStream, xs: Any *) {
    out.write(ocd.encode(xs
      .flatMap { case x: Seq[_] => x case x: Any => Seq(x) }
      .map { s => escape(s.toString) }
      .mkString(ofs)))
  }

  def writeln(out: OutputStream, xs: Any *) {
    write(out, xs: _*)
    out.write(ocd.encode(ors))
  }
}

object Writer {
  def apply() = new Writer()
}
