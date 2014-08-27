
package wok.reflect

import util.DynamicVariable
import util.matching.Regex
import scalax.io.Codec
import wok.core.Stdio.{out => STDOUT}
import wok.csv.{Writer, Reader, Quote}


trait AbstractWok {
  val args: List[String]

  def runScript(): Unit

  implicit val wokInstance: AbstractWok = this

  def print(x: Any *): Unit = STDOUT.print(x: _*)
  def printf(x: Any *): Unit = STDOUT.printf(x: _*)
  def println(x: Any *): Unit = STDOUT.println(x: _*)

  val _reader = new DynamicVariable[Reader](Reader())
  val _writer = new DynamicVariable[Writer](Writer())

  def reader = _reader.value
  def writer = _writer.value

  def Guard[T](f: => T): T = {
    _reader.withValue(reader.copy()) {
      _writer.withValue(writer.copy()) {
        f
      }
    }
  }

  def Reset[T](f: => T): T = {
    _reader.withValue(Reader()) {
      _writer.withValue(Writer()) {
        f
      }
    }
  }

  def FS: Regex = reader.FS
  def RS: Regex = reader.RS
  def FQ: Quote = reader.FQ
  def CD: Codec = reader.CD

  def FS(r: Regex) = { reader.FS(r); this }
  def RS(r: Regex) = { reader.RS(r); this }
  def FS(s: String) = { reader.FS(s); this }
  def RS(s: String) = { reader.RS(s); this }
  def FS(c: Char) = { reader.FS(c); this }
  def RS(c: Char) = { reader.RS(c); this }
  def FQ(q: Quote) = { reader.FQ(q); this }
  def CD(c: Codec) = { reader.CD(c); this }

  def OFS: String = writer.OFS
  def ORS: String = writer.ORS
  def OFQ: Quote = writer.OFQ
  def OCD: Codec = writer.OCD

  def OFS(s: String) = { writer.OFS(s); this }
  def ORS(s: String) = { writer.ORS(s); this }
  def OFS(c: Char) = { writer.OFS(c); this }
  def ORS(c: Char) = { writer.ORS(c); this }
  def OFQ(q: Quote) = { writer.OFQ(q); this }
  def OCD(c: Codec) = { writer.OCD(c); this }
}
