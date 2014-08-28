
package wok.reflect

import java.io.InputStream
import scalax.file.Path
import util.DynamicVariable
import util.matching.Regex
import scalax.io.Codec
import wok.core.Stdio.{out => STDOUT}
import wok.csv.{Row, Writer, Reader, Quote}


trait AbstractWok {
  val args: List[String]

  def runScript(): Unit

  implicit val wokInstance: AbstractWok = this

  def print(x: Any *): Unit = STDOUT.print(x: _*)
  def printf(x: Any *): Unit = STDOUT.printf(x: _*)
  def println(x: Any *): Unit = STDOUT.println(x: _*)

  val _ARGV = new DynamicVariable[List[String]](Nil)
  val _ARGC = new DynamicVariable[Int](0)
  val _ARGIND = new DynamicVariable[Int](0)
  val _FILENAME = new DynamicVariable[String]("")
  val _FNR = new DynamicVariable[Long](0)
  val _NR = new DynamicVariable[Long](0)
  val _NF = new DynamicVariable[Int](0)
  val _FT = new DynamicVariable[List[String]](Nil)
  val _RT = new DynamicVariable[String]("")
  val _READER = new DynamicVariable[Reader](Reader())
  val _WRITER = new DynamicVariable[Writer](Writer())

  def ARGV = _ARGV.value
  def ARGC = _ARGC.value
  def ARGIND = _ARGIND.value
  def FILENAME = _FILENAME.value
  def FNR = _FNR.value
  def NR = _NR.value
  def NF = _NF.value
  def FT = _FT.value
  def RT = _RT.value
  def READER = _READER.value
  def WRITER = _WRITER.value

  def FS: Regex = READER.FS
  def RS: Regex = READER.RS
  def FQ: Quote = READER.FQ
  def CD: Codec = READER.CD

  def FS(r: Regex) = { READER.FS(r); this }
  def RS(r: Regex) = { READER.RS(r); this }
  def FS(s: String) = { READER.FS(s); this }
  def RS(s: String) = { READER.RS(s); this }
  def FS(c: Char) = { READER.FS(c); this }
  def RS(c: Char) = { READER.RS(c); this }
  def FQ(q: Quote) = { READER.FQ(q); this }
  def CD(c: Codec) = { READER.CD(c); this }

  def OFS: String = WRITER.OFS
  def ORS: String = WRITER.ORS
  def OFQ: Quote = WRITER.OFQ
  def OCD: Codec = WRITER.OCD

  def OFS(s: String) = { WRITER.OFS(s); this }
  def ORS(s: String) = { WRITER.ORS(s); this }
  def OFS(c: Char) = { WRITER.OFS(c); this }
  def ORS(c: Char) = { WRITER.ORS(c); this }
  def OFQ(q: Quote) = { WRITER.OFQ(q); this }
  def OCD(c: Codec) = { WRITER.OCD(c); this }

  trait InputProcessor[-T] {
    def process[A](xs: T *)(f: Row => A): Iterator[A]
    def process[A](files: List[String], streams: List[InputStream])(f: Row => A): Iterator[A] = {
      val reader = READER.copy()
      val writer = WRITER.copy()
      var __NR = -1
      streams
        .map (reader.open)
        .toIterator
        .zipWithIndex
        .map { case (itr, i) => itr map { row =>
          _READER.withValue(reader) {
            _WRITER.withValue(writer) {
              _ARGV.withValue(files) {
                _ARGC.withValue(files.size) {
                  _ARGIND.withValue(i) {
                    _FILENAME.withValue(files(i)) {
                      __NR += 1
                      _NR.withValue(__NR) {
                        _FNR.withValue(row.id) {
                          _NF.withValue(row.size) {
                            _FT.withValue(row.sep) {
                              _RT.withValue(row.term) {
                                f(row)
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      .flatten
    }
  }

  implicit object StreamInputProcessor extends InputProcessor[InputStream] {
    def process[A](streams: InputStream *)(f: Row => A) =
      process(streams map (x => "-") toList, streams.toList)(f)
  }

  implicit object PathStringInputProcessor extends InputProcessor[String] {
    def process[A](files: String *)(f: Row => A) =
      process(files.toList, files map (file => Path.fromString(file).inputStream.open().get) toList)(f)
  }

  def In[T: InputProcessor, A](xs: T *)(f: Row => A) = implicitly[InputProcessor[T]].process(xs: _*)(f)
}
