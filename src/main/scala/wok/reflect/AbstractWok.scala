
package wok.reflect

import java.io.InputStream
import util.matching.Regex
import scalax.io.Codec
import scalax.file.Path
import wok.core.{ThreadSafeVariable => ThreadSafe, ThreadSafeExecutor}
import wok.core.Stdio.{in => Stdin, out => Stdout}
import wok.csv.{Writer, Reader, Quote}


trait AbstractWok {
  val args: List[String] = Nil

  def runScript(): Unit

  implicit val wokInstance: AbstractWok = this

  def print(x: Any *): Unit = Stdout.print(x: _*)
  def printf(x: Any *): Unit = Stdout.printf(x: _*)
  def println(x: Any *): Unit = Stdout.println(x: _*)

  lazy val _TS0 = new ThreadSafe[TS0](TS0(Reader(), Writer(), args, args.size))
    .withCopy (v => v.copy(reader = v.reader.copy(), writer = v.writer.copy()))
  val _TS1 = new ThreadSafe[TS1](TS1(0, ""))
  val _TS2 = new ThreadSafe[TS2](TS2(0, 0, 0, Nil, "", Nil))

  def READER = _TS0.value.reader
  def WRITER = _TS0.value.writer
  def ARGV = _TS0.value.argv
  def ARGC = _TS0.value.argc
  def ARGIND = _TS1.value.argind
  def FILENAME = _TS1.value.filename
  def FNR = _TS2.value.fnr
  def NR = _TS2.value.nr
  def NF = _TS2.value.nf
  def FT = _TS2.value.ft
  def RT = _TS2.value.rt
  def ROW = _TS2.value.row
  def $0 = FT .zipWithIndex .map {case (s, i) => ROW(i) + s } .flatten .mkString + ROW.last + RT

  def FS: Regex = READER.FS
  def RS: Regex = READER.RS
  def FQ: Quote = READER.FQ
  def CD: Codec = READER.CD

  def FS_=(r: Regex): Unit = READER.FS(r)
  def RS_=(r: Regex): Unit = READER.RS(r)
  def FS_=(s: String): Unit = READER.FS(s)
  def RS_=(s: String): Unit = READER.RS(s)
  def FS_=(c: Char): Unit = READER.FS(c)
  def RS_=(c: Char): Unit = READER.RS(c)
  def FQ_=(q: Quote): Unit = READER.FQ(q)
  def CD_=(c: Codec): Unit = READER.CD(c)

  def OFS: String = WRITER.OFS
  def ORS: String = WRITER.ORS
  def OFQ: Quote = WRITER.OFQ
  def OCD: Codec = WRITER.OCD

  def OFS_=(s: String): Unit = WRITER.OFS(s)
  def ORS_=(s: String): Unit = WRITER.ORS(s)
  def OFS_=(c: Char): Unit = WRITER.OFS(c)
  def ORS_=(c: Char): Unit = WRITER.ORS(c)
  def OFQ_=(q: Quote): Unit = WRITER.OFQ(q)
  def OCD_=(c: Codec): Unit = WRITER.OCD(c)

  case class TS0(reader: Reader, writer: Writer, argv: List[String], argc: Int)
  case class TS1(argind: Int, filename: String)
  case class TS2(nr: Int, fnr: Int, nf: Int, ft: List[String], rt: String, row: List[String])

  trait InputProcessor[-T] {
    def process[A](xs: T *)(f: Iterator[List[String]] => A): A
    def process[A](files: List[String], streams: List[InputStream])(f: Iterator[List[String]] => A): A = {
      // backup ThreadSafe variables
      val __TS0 = _TS0.value
      val __TS1 = _TS1.value
      val __TS2 = _TS2.value
      // use copied reader/writer to guard inherited reader/writer
      _TS0.value = TS0(__TS0.reader.copy(), __TS0.writer.copy(), files, files.size)
      var totalId = -1
      val itr = streams
        .map (READER.open)
        .toIterator
        .zipWithIndex
        .map { case (rows, i) =>
          _TS1.value = TS1(i, files(i))
          rows map { row =>
            totalId += 1
            _TS2.value = TS2(totalId, row.id, row.get.size, row.sep, row.term, row.get)
            row.get
          }
        }
      .flatten

      try f(itr)
      finally {
        // restore ThreadSafe variables
        _TS0.value = __TS0
        _TS1.value = __TS1
        _TS2.value = __TS2
      }
    }
  }

  implicit object StreamInputProcessor extends InputProcessor[InputStream] {
    def process[A](streams: InputStream *)(f: Iterator[List[String]] => A): A =
      process(streams map (x => "-") toList, streams.toList)(f)
  }

  implicit object PathStringInputProcessor extends InputProcessor[String] {
    def process[A](files: String *)(f: Iterator[List[String]] => A): A =
      process(files.toList, files map (file => Path.fromString(file).inputStream.open().get) toList)(f)
  }

  object In {
    def apply[A](f: Iterator[List[String]] => A): A = {
      if (ARGC == 0)
        from(Stdin.inputStream.open().get)(f)
      else
        from(ARGV: _*)(f)
    }
    def from[T: InputProcessor, A](xs: T *)(f: Iterator[List[String]] => A): A =
      implicitly[InputProcessor[T]].process(xs: _*)(f)
  }

  implicit val executionContext = scala.concurrent.ExecutionContext.fromExecutorService(new ThreadSafeExecutor)
}
