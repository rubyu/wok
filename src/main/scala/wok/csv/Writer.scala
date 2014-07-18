
package wok.csv

import java.io.OutputStream
import wok.core.Helpers._
import scalax.io.Codec


class Writer {
  private var ofs = " "
  private var ors = "\n"
  private var ofq = QuoteOption()
  private var ocd = Codec.default

  def OFS = ofs
  def ORS = ors
  def OFQ = ofq
  def OCD = ocd

  def OFS(s: String) = { ofs = s; update(); this }
  def ORS(s: String) = { ors = s; update(); this }
  def OFQ(q: QuoteOption) = { ofq = q; update(); this }
  def OCD(c: Codec) = { ocd = c; this }

  private def _escape: String => String = {
    ofq match {
      //escapes e and q in a given string with e and quotes it with q
      case QuoteOption(QuoteAll, Some(q), Some(e)) => { _.escaped(e, q).quoted(q) }

      //quotes a given string with q
      case QuoteOption(QuoteAll, Some(q), None) => { _.escaped(q).quoted(q) }

      //escapes e and q in a given string with e and quotes it with q when it contains OFS or ORS
      case QuoteOption(QuoteMin, Some(q), Some(e)) => {
        case s if s.contains(ofs) || s.contains(ors) => s.escaped(e, q).quoted(q)
        case s => s.escaped(e, q)
      }

      //escapes q in a given string with q and quotes it with q when it contains OFS or ORS or q
      case QuoteOption(QuoteMin, Some(q), None) => {
        case s if s.contains(ofs) || s.contains(ors) || s.contains(q) => s.escaped(q).quoted(q)
        case s => s
      }

      //escapes OFS in a given string with e and throws an error when it contains ORS
      case QuoteOption(QuoteNone, _, Some(e)) => {
        case s if s.contains(ors) => throw new RuntimeException
        case s => s.escaped(e, ofs)
      }

      //throws an error when a given string contains OFS or ORS
      case QuoteOption(QuoteNone, _, None) => {
        case s if s.contains(ofs) || s.contains(ors) => throw new RuntimeException
        case s => s
      }
    }
  }

  private var escape = _escape
  private def update(): Unit = escape = _escape

  def write(out: OutputStream, x: Any) {
    val str = x match {
      case x: Seq[_] => x.map { x => escape(x.toString) } mkString(OFS)
      case x => escape(x.toString)
    }
    out.write(ocd.encode(str))
  }

  def writeln(out: OutputStream, x: Any) {
    write(out, x)
    out.write(ocd.encode(ors))
  }
}