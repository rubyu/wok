
package wok.core

import util.matching.Regex
import wok.csv.{Writer, Reader, QuoteOption}
import java.nio.charset.Charset
import scalax.io.Codec


trait AbstractWok {
  def arg: List[String]

  def print(x: Any)(implicit w: Writer): Unit = { w.write(Console.out, x) }
  def println(x: Any = "")(implicit w: Writer): Unit = { w.writeln(Console.out, x) }

  def reader: Reader
  implicit def writer: Writer

  def FS: Regex = reader.FS
  def RS: Regex = reader.RS
  def FQ: QuoteOption = reader.FQ
  def CD: Codec = reader.CD

  def FS(r: Regex) = { reader.FS(r); this }
  def RS(r: Regex) = { reader.RS(r); this }
  def FQ(q: QuoteOption) = { reader.FQ(q); this }
  def CD(c: Charset) = { reader.CD(c); this }

  def OFS: String = writer.OFS
  def ORS: String = writer.ORS
  def OFQ: QuoteOption = writer.OFQ
  def OCD: Codec = writer.OCD

  def OFS(s: String) = { writer.OFS(s); this }
  def ORS(s: String) = { writer.ORS(s); this }
  def OFQ(q: QuoteOption) = { writer.OFQ(q); this }
  def OCD(c: Charset) = { writer.OCD(c); this }
}
