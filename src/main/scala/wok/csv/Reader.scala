
package wok.csv

import util.matching.Regex
import wok.core.Helpers._
import java.nio.charset.{Charset, StandardCharsets}
import java.io.{InputStreamReader, BufferedReader, InputStream}


class Reader {
  private var fs = """\s+""".r
  private var rs = """(\r\n|\r|\n)""".r
  private var fq = QuoteOption()
  private var cd = StandardCharsets.UTF_8

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

  def FQ(q: QuoteOption) = { fq = q; update(); this }
  def CD(c: Charset) = { cd = c; this }

  def open(in: InputStream) = new RowIterator(new BufferedReader(new InputStreamReader(in, cd)), this)
  def open(in: java.io.Reader) = new RowIterator(in, this)
}