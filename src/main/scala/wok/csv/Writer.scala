
package wok.csv

import java.io.OutputStream
import wok.core.Helpers._
import scala.util.matching.Regex.quoteReplacement //quoteReplacement(str) == str.escaped('\\', '$')
import scalax.io.Codec


class Writer {
  private var ofs = " "
  private var ors = "\n"
  private var ofq = Quote.None()
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
      /** Quotes a given string with Q. */
      case Quote(Quote.Mode.All, Some(q), _) => _.escaped(q).quoted(q)

      /** Escapes Q in a given string with Q and quotes it with Q when it contains ORS or OFS or Q or E. */
      case Quote(Quote.Mode.Min, Some(q), Some(e)) =>
        val p =  s"""(${ors.er}|${ofs.er}|${q.er}|${e.er})""".r
        s => p.findFirstMatchIn(s) match {
          case Some(_) => s.escaped(q).quoted(q)
          case None => s
        }

      /** Escapes Q in a given string with Q and quotes it with Q when it contains ORS or OFS or Q. */
      case Quote(Quote.Mode.Min, Some(q), None) =>
        val p =  s"""(${ors.er}|${ofs.er}|${q.er})""".r
        s => p.findFirstMatchIn(s) match {
          case Some(_) => s.escaped(q).quoted(q)
          case None => s
        }

      /** Escapes ORS, OFS, E and Q. */
      case Quote(Quote.Mode.None, Some(q), Some(e)) =>
        val p =  s"""(${ors.er}|${ofs.er}|${q.er}|${e.er})""".r
        val r = quoteReplacement(e.toString) + "$0"
        p.replaceAllIn(_, r)

      /** Escapes ORS, OFS and E. */
      case Quote(Quote.Mode.None, None, Some(e)) =>
        val p =  s"""(${ors.er}|${ofs.er}|${e.er})""".r
        val r = quoteReplacement(e.toString) + "$0"
        p.replaceAllIn(_, r)

      /** Returns a given string as is. Throws an EncodingException when it contains ORS or OFS or Q. */
      case Quote(Quote.Mode.None, Some(q), None) =>
        val p = s"""(${ors.er}|${ofs.er}|${q.er})""".r
        s => p.findFirstMatchIn(s) match {
          case Some(m) => throw new EncodingException(s"Field values must not contain ORS, OFS, and Q")
          case None => s
        }

      /** Returns a given string as is. Throws an EncodingException when it contains ORS or OFS. */
      case Quote(Quote.Mode.None, None, None) =>
        val p = s"""(${ors.er}|${ofs.er})""".r
        s => p.findFirstMatchIn(s) match {
          case Some(m) => throw new EncodingException(s"Field values must not contain ORS and OFS")
          case None => s
        }
    }
  }

  private var escape = _escape
  private def update(): Unit = escape = _escape

  def write(out: OutputStream, xs: Any *): Unit = {
    out.write(ocd.encode(
      xs.map( x => escape(x.toString) ).mkString(ofs)
    ))
  }

  def writeln(out: OutputStream, xs: Any *): Unit = {
    write(out, xs: _*)
    out.write(ocd.encode(ors))
  }

  def copy(OFS: String = this.ofs, ORS: String = this.ors, OFQ: Quote = this.ofq, OCD: Codec = this.ocd): Writer = {
    new Writer().OFS(OFS).ORS(ORS).OFQ(OFQ).OCD(OCD)
  }

  override def equals(other: Any) = other match {
    case that: Writer if that.isInstanceOf[Writer] =>
      this.OFS == that.OFS && this.ORS == that.ORS && this.OFQ == that.OFQ && this.OCD.name == that.OCD.name
    case _ => false
  }
}

object Writer {
  def apply() = new Writer()

  def OFS = new Writer().OFS
  def ORS = new Writer().ORS
  def OFQ = new Writer().OFQ
  def OCD = new Writer().OCD

  def OFS(s: String) = new Writer().OFS(s)
  def ORS(s: String) = new Writer().ORS(s)
  def OFQ(q: Quote) = new Writer().OFQ(q)
  def OCD(c: Codec) = new Writer().OCD(c)

  def write(out: OutputStream, xs: Any *) = new Writer().write(out, xs: _*)
  def writeln(out: OutputStream, xs: Any *) = new Writer().writeln(out, xs: _*)
}
