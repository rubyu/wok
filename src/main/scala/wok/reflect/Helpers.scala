
package wok.reflect

import scalax.file.Path
import wok.csv.{Row, Reader}
import scalax.file.defaultfs.{DefaultPath, RedirectModePath => RMPath, AppendModePath => AMPath}
import scalax.io.managed.{OutputStreamResource, InputStreamResource}
import scalax.io.{StandardOpenOption, Codec}
import java.io.{OutputStream, InputStream}


object Helpers {
  implicit class ExtendedInputStream(val in: InputStream) extends AnyVal {
    def csv(implicit wok: AbstractWok): Iterator[Row] = wok.reader.open(in)
  }

  implicit class ExtendedOutputStream(val out: OutputStream) extends AnyVal {
    def print(x: Any *)(implicit wok: AbstractWok): Unit = wok.writer.write(out, x: _*)
    def printf(x: Any *)(implicit wok: AbstractWok): Unit = wok.writer.writeField(out, x: _*)
    def println(x: Any *)(implicit wok: AbstractWok): Unit = wok.writer.writeRow(out, x: _*)
  }

  implicit class ExtendedInputStreamResource(val in: InputStreamResource[InputStream]) extends AnyVal {
    def #>[A](f: InputStream => A): A = in.acquireAndGet(f)
  }

  implicit class ExtendedOutputStreamResource(val out: OutputStreamResource[OutputStream]) extends AnyVal {
    def #<<[A](f: OutputStream => A): A = out.acquireAndGet(f)
  }

  implicit class ExtendedPath[T <: Path](val path: T) extends AnyVal {
    def #>[A](f: InputStream => A): A = path.inputStream().acquireAndGet(f)
    def `!<<` = new AMPath(path.asInstanceOf[DefaultPath])
    def #<<[A](f: OutputStream => A): A = path.outputStream(StandardOpenOption.Append).acquireAndGet(f)
    def `!<` = new RMPath(path.asInstanceOf[DefaultPath])
    def #<[A](f: OutputStream => A): A = path.outputStream(StandardOpenOption.Write).acquireAndGet(f)
  }

  implicit class ExtendedPathString(val s: String) extends AnyVal {
    def #>[A](f: InputStream => A): A = Path.fromString(s).inputStream.acquireAndGet(f(_))
    def `!<<` = new AMPath(Path.fromString(s))
    def #<<[A](f: OutputStream => A): A = Path.fromString(s).outputStream(StandardOpenOption.Append).acquireAndGet(f)
    def `!<` = new RMPath(Path.fromString(s))
    def #<[A](f: OutputStream => A): A = Path.fromString(s).outputStream(StandardOpenOption.Write).acquireAndGet(f)
  }

  implicit def codecToCharset(c: Codec) = c.charSet
}
