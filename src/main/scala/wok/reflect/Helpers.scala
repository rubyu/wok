
package wok.reflect

import scalax.file.Path
import wok.csv.{Row, Reader, Writer}
import scalax.file.defaultfs.{DefaultPath, RedirectModePath => RMPath, AppendModePath => AMPath}
import scalax.io.managed.{OutputStreamResource, InputStreamResource}
import scalax.io.{StandardOpenOption, Codec}
import java.io.{OutputStream, InputStream}


object Helpers {
  implicit class OpenableInputStream(val in: InputStream) extends AnyVal {
    def csv(implicit r: Reader): Iterator[Row] = r.open(in)
  }

  implicit class OpenableInputStreamResource(val in: InputStreamResource[InputStream]) extends AnyVal {
    def csv(implicit r: Reader): Iterator[Row] = in.open().get.csv(r)
  }

  implicit class OpenablePath(val p: Path) extends AnyVal {
    def csv(implicit r: Reader): Iterator[Row] = p.inputStream().csv(r)
  }

  implicit class OpenableString(val s: String) extends AnyVal {
    def csv(implicit r: Reader): Iterator[Row] = Path.fromString(s).csv(r)
  }

  implicit class PrintableOutputStream(val out: OutputStream) extends AnyVal {
    def print(x: Any *)(implicit w: Writer): Unit = w.write(out, x: _*)
    def println(x: Any *)(implicit w: Writer): Unit = w.writeln(out, x: _*)
  }

  implicit class PrintableOutputStreamResource(val out: OutputStreamResource[OutputStream]) extends AnyVal {
    def print(x: Any *)(implicit w: Writer): Unit = out.acquireAndGet{ _.print(x: _*)(w) }
    def println(x: Any *)(implicit w: Writer): Unit = out.acquireAndGet{ _.println(x: _*)(w) }
  }

  implicit class AppendModePath[T <: Path](val path: T) extends AnyVal {
    def <<| = new AMPath(path.asInstanceOf[DefaultPath])
    def <<|[A](f: OutputStream => A): A = path.outputStream(StandardOpenOption.Append).acquireAndGet(f(_))
  }

  implicit class RedirectModePath[T <: Path](val path: T) extends AnyVal {
    def <| = new RMPath(path.asInstanceOf[DefaultPath])
    def <|[A](f: OutputStream => A): A = path.outputStream(StandardOpenOption.Write).acquireAndGet(f(_))
  }

  implicit class AppendModePathString(val s: String) extends AnyVal {
    def <<| = new AMPath(Path.fromString(s))
    def <<|[A](f: OutputStream => A): A = Path.fromString(s).outputStream(StandardOpenOption.Append).acquireAndGet(f(_))
  }

  implicit class RedirectModePathString(val s: String) extends AnyVal {
    def <| = new RMPath(Path.fromString(s))
    def <|[A](f: OutputStream => A): A = Path.fromString(s).outputStream(StandardOpenOption.Write).acquireAndGet(f(_))
  }

  implicit def codecToCharset(c: Codec) = c.charSet
}
