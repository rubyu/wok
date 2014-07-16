
package wok.csv

import util.matching.Regex
import wok.core.Helpers._


class Reader {
  //todo support codec
  //private var cd =
  private var fs = """\s+""".r
  private var rs = """(\r\n|\r|\n)""".r
  private var fq = QuoteOption()
  private var parser = new Parser(fs, rs, fq)

  private def update() { parser = new Parser(fs, rs, fq) }

  def parse(in: CharSequence) = parser.parse(in)

  def FS = fs
  def RS = rs
  def FQ = fq

  def FS(r: Regex) = { fs = r; update(); this }
  def RS(r: Regex) = { rs = r; update(); this }

  def FS(s: String) = { fs = s.er; update(); this }
  def RS(s: String) = { rs = s.er; update(); this }

  def FQ(q: QuoteOption) = { fq = q; update(); this }

  def open(in: java.io.Reader) = new RowIterator(in, this)
}