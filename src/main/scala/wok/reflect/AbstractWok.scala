
package wok.reflect

import util.matching.Regex
import wok.csv.{Writer, Reader, Quote}
import scalax.io.Codec
import java.io.OutputStream
import Helpers.PrintableOutputStream


trait AbstractWok {
  val arg: List[String]

  def runScript(): Unit

  def print(x: Any *)(implicit w: Writer): Unit = { Console.out.asInstanceOf[OutputStream].print(x: _*)(w) }
  def println(x: Any *)(implicit w: Writer): Unit = { Console.out.asInstanceOf[OutputStream].println(x: _*)(w) }

  implicit val defaultReader = Reader()
  implicit val defaultWriter = Writer()

  def FS: Regex = defaultReader.FS
  def RS: Regex = defaultReader.RS
  def FQ: Quote = defaultReader.FQ
  def CD: Codec = defaultReader.CD

  def FS(r: Regex) = { defaultReader.FS(r); this }
  def RS(r: Regex) = { defaultReader.RS(r); this }
  def FS(s: String) = { defaultReader.FS(s); this }
  def RS(s: String) = { defaultReader.RS(s); this }
  def FQ(q: Quote) = { defaultReader.FQ(q); this }
  def CD(c: Codec) = { defaultReader.CD(c); this }

  def OFS: String = defaultWriter.OFS
  def ORS: String = defaultWriter.ORS
  def OFQ: Quote = defaultWriter.OFQ
  def OCD: Codec = defaultWriter.OCD

  def OFS(s: String) = { defaultWriter.OFS(s); this }
  def ORS(s: String) = { defaultWriter.ORS(s); this }
  def OFQ(q: Quote) = { defaultWriter.OFQ(q); this }
  def OCD(c: Codec) = { defaultWriter.OCD(c); this }
}
