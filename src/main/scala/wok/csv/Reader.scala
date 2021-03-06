
package wok.csv

import util.matching.Regex
import wok.core.Helpers._
import java.io.{InputStreamReader, BufferedReader, InputStream}
import scalax.io.Codec


class Reader {
  private var fs = """[ \t]+""".r
  private var rs = """\r\n|\r|\n""".r
  private var fq = Quote.None()
  private var cd = Codec.default

  private var parser = new Parser(fs, rs, fq)

  private def update() { parser = new Parser(fs, rs, fq) }

  def parse(in: CharSequence) = parser.parse(in)

  def FS = fs
  def RS = rs
  def FQ = fq
  def CD = cd

  def FS(r: Regex) = { fs = r; update(); this }
  def RS(r: Regex) = { rs = r; update(); this }

  private val matchNothing = """(?!.).""".r

  def FS(s: String): Reader = FS(if (s.isEmpty) matchNothing else s.er)
  def RS(s: String): Reader = RS(if (s.isEmpty) matchNothing else s.er)

  def FS(c: Char): Reader = FS(c.er)
  def RS(c: Char): Reader = RS(c.er)

  def FQ(q: Quote) = { fq = q; update(); this }

  /** Restriction: This method do not reflect the changes of CD to the action of a RowIterator opened before. */
  def CD(c: Codec) = { cd = c; this }

  def open(in: InputStream) = new RowIterator(new BufferedReader(new InputStreamReader(in, cd.charSet)), this)

  def copy(FS: Regex = this.fs, RS: Regex = this.rs, FQ: Quote = this.fq, CD: Codec = this.cd): Reader = {
    new Reader().FS(FS).RS(RS).FQ(FQ).CD(CD)
  }

  override def equals(other: Any) = other match {
    case that: Reader if that.isInstanceOf[Reader] =>
      this.FS.toString() == that.FS.toString() && this.RS.toString() == that.RS.toString() &&
        this.FQ == that.FQ && this.CD.name == that.CD.name
    case _ => false
  }
}

object Reader {
  def apply() = new Reader()

  def FS = new Reader().FS
  def RS = new Reader().RS
  def FQ = new Reader().FQ
  def CD = new Reader().CD

  def FS(c: Char) = new Reader().FS(c)
  def RS(c: Char) = new Reader().RS(c)
  def FS(s: String) = new Reader().FS(s)
  def RS(s: String) = new Reader().RS(s)
  def FS(r: Regex) = new Reader().FS(r)
  def RS(r: Regex) = new Reader().RS(r)
  def FQ(q: Quote) = new Reader().FQ(q)
  def CD(c: Codec) = new Reader().CD(c)

  def open(in: InputStream) = new Reader().open(in)
}
