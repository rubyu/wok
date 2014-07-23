
package wok.csv

import util.matching.Regex
import wok.core.Helpers._
import java.io.{InputStreamReader, BufferedReader, InputStream}
import scalax.io.Codec


class Reader {
  private var fs = """[ \t]+""".r
  private var rs = """\r\n|\r|\n""".r
  private var fq = Quote()
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

  def FS(s: String) = {
    fs = if (s.isEmpty) matchNothing else s.er
    update()
    this
  }

  def RS(s: String) = {
    rs = if (s.isEmpty) matchNothing else s.er
    update()
    this
  }

  def FQ(q: Quote) = { fq = q; update(); this }

  /** Restriction: This method do not reflect the changes of CD to the action of a RowIterator opened before. */
  def CD(c: Codec) = { cd = c; this }

  def open(in: InputStream) = new RowIterator(new BufferedReader(new InputStreamReader(in, cd.charSet)), this)
}

object Reader {
  def apply() = new Reader()

  def FS = new Reader().FS
  def RS = new Reader().RS
  def FQ = new Reader().FQ
  def CD = new Reader().CD

  def FS(s: String) = new Reader().FS(s)
  def RS(s: String) = new Reader().RS(s)
  def FQ(q: Quote) = new Reader().FQ(q)
  def CD(c: Codec) = new Reader().CD(c)

  def open(in: InputStream) = new Reader().open(in)
}
