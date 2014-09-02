
package wok.reflect

import java.io.InputStream
import util.matching.Regex
import scalax.io.Codec
import scalax.file.Path
import wok.core.{ThreadSafeVariable => ThreadSafe, ThreadSafeExecutor}
import wok.core.Stdio.{in => Stdin, out => Stdout}
import wok.csv.{Row, Writer, Reader, Quote}


trait AbstractWok {
  val args: List[String] = Nil

  def runScript(): Unit

  implicit val wokInstance: AbstractWok = this

  def print(x: Any *): Unit = Stdout.print(x: _*)
  def printf(x: Any *): Unit = Stdout.printf(x: _*)
  def println(x: Any *): Unit = Stdout.println(x: _*)

  lazy val _ARGV = new ThreadSafe[List[String]](args)
  lazy val _ARGC = new ThreadSafe[Int](args.size)
  val _ARGIND = new ThreadSafe[Int](0)
  val _FILENAME = new ThreadSafe[String]("")
  val _FNR = new ThreadSafe[Long](0)
  val _NR = new ThreadSafe[Long](0)
  val _NF = new ThreadSafe[Int](0)
  val _FT = new ThreadSafe[List[String]](Nil)
  val _RT = new ThreadSafe[String]("")
  val _READER = new ThreadSafe[Reader](Reader()) withCopy(v => v.copy())
  val _WRITER = new ThreadSafe[Writer](Writer()) withCopy(v => v.copy())

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
    def process[A](xs: T *)(f: Iterator[Row] => A): A
    def process[A](files: List[String], streams: List[InputStream])(f: Iterator[Row] => A): A = {
      // backup dynamic variables
      val __ARGV = ARGV
      val __ARGC = ARGC
      val __ARGIND = ARGIND
      val __FILENAME = FILENAME
      val __FNR = FNR
      val __NR = NR
      val __NF = NF
      val __FT = FT
      val __RT = RT
      val __READER = READER
      val __WRITER = WRITER

      // use copied reader/writer to guard inherited reader/writer
      _READER.value = __READER.copy()
      _WRITER.value = __WRITER.copy()

      // update dynamic variables
      _ARGV.value = files
      _ARGC.value = files.size
      _NR.value = -1

      val itr = streams
        .map (READER.open)
        .toIterator
        .zipWithIndex
        .map { case (rows, i) =>
          _ARGIND.value = i
          _FILENAME.value = files(i)
          rows map { row =>
            _NR.value += 1
            _FNR.value = row.id
            _NF.value = row.size
            _FT.value = row.sep
            _RT.value = row.term
            row
          }
        }
      .flatten

      try f(itr)
      finally {
        // restore dynamic variables
        _ARGV.value = __ARGV
        _ARGC.value = __ARGC
        _ARGIND.value = __ARGIND
        _FILENAME.value = __FILENAME
        _FNR.value = __FNR
        _NR.value = __NR
        _NF.value = __NF
        _FT.value = __FT
        _RT.value = __RT
        _READER.value = __READER
        _WRITER.value = __WRITER
      }
    }
  }

  implicit object StreamInputProcessor extends InputProcessor[InputStream] {
    def process[A](streams: InputStream *)(f: Iterator[Row] => A): A =
      process(streams map (x => "-") toList, streams.toList)(f)
  }

  implicit object PathStringInputProcessor extends InputProcessor[String] {
    def process[A](files: String *)(f: Iterator[Row] => A): A =
      process(files.toList, files map (file => Path.fromString(file).inputStream.open().get) toList)(f)
  }

  object In {
    def apply[A](f: Iterator[Row] => A): A = {
      if (ARGC == 0)
        from(Stdin.inputStream.open().get)(f)
      else
        from(ARGV: _*)(f)
    }
    def from[T: InputProcessor, A](xs: T *)(f: Iterator[Row] => A): A = implicitly[InputProcessor[T]].process(xs: _*)(f)
  }

  implicit val executionContext = scala.concurrent.ExecutionContext.fromExecutorService(new ThreadSafeExecutor)
}
