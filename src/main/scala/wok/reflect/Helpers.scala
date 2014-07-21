
package wok.reflect

import scalax.file.Path
import wok.csv.{Row, Reader, Writer}
import scalax.io.{Resource, StandardOpenOption}
import java.io.{OutputStream, InputStream, FileNotFoundException}


object Helpers {
  implicit class OpenableInputStream(val in: InputStream) extends AnyVal {
    def open()(implicit r: Reader): Iterator[Row] = r.open(in)
  }

  implicit class OpenableReader(val reader: java.io.Reader) extends AnyVal {
    def open()(implicit r: Reader): Iterator[Row] = r.open(reader)
  }

  implicit class OpenablePath(val p: Path) extends AnyVal {
    def open()(implicit r: Reader): Iterator[Row] = {
      if (p.exists) Resource.fromFile(p.fileOption.get).inputStream.open().get.open()(r)
      else throw new FileNotFoundException()
    }
  }

  implicit class PrintableOutputStream(val out: OutputStream) extends AnyVal {
    def print(x: Any *)(implicit w: Writer): Unit = w.write(out, x: _*)
    def println(x: Any *)(implicit w: Writer): Unit = w.writeln(out, x: _*)
  }

  implicit class PrintablePath(val p: Path) extends AnyVal {
    def print(x: Any *)(implicit w: Writer): Unit =
      p.outputStream(StandardOpenOption.Append).acquireFor { _.print(x: _*)(w) }

    def println(x: Any *)(implicit w: Writer): Unit =
      p.outputStream(StandardOpenOption.Append).acquireFor { _.println(x: _*)(w) }
  }

  implicit class CompletableAny(val u: Any) extends AnyVal {
    def complete(): Unit = {}
  }

  implicit class CompletableIterator(val itr: Iterator[_]) extends AnyVal {
    def complete(): Unit = itr.foreach { x => }
  }
}
