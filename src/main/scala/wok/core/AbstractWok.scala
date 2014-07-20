
package wok.core

import util.matching.Regex
import wok.csv.{Writer, Reader, QuoteOption}
import scalax.io.Codec


trait AbstractWok {
  val arg: List[String]

  def print(x: Any *)(implicit w: Writer): Unit = { w.write(Console.out, x: _*) }
  def println(x: Any *)(implicit w: Writer): Unit = { w.writeln(Console.out, x: _*) }

  implicit val reader: Reader
  implicit val writer: Writer

  def FS: Regex = reader.FS
  def RS: Regex = reader.RS
  def FQ: QuoteOption = reader.FQ
  def CD: Codec = reader.CD

  def FS(r: Regex) = { reader.FS(r); this }
  def RS(r: Regex) = { reader.RS(r); this }
  def FS(s: String) = { reader.FS(s); this }
  def RS(s: String) = { reader.RS(s); this }
  def FQ(q: QuoteOption) = { reader.FQ(q); this }
  def CD(c: Codec) = { reader.CD(c); this }

  def OFS: String = writer.OFS
  def ORS: String = writer.ORS
  def OFQ: QuoteOption = writer.OFQ
  def OCD: Codec = writer.OCD

  def OFS(s: String) = { writer.OFS(s); this }
  def ORS(s: String) = { writer.ORS(s); this }
  def OFQ(q: QuoteOption) = { writer.OFQ(q); this }
  def OCD(c: Codec) = { writer.OCD(c); this }
}
