
package scalax.io

import wok.csv.Writer
import wok.reflect.Helpers.ExtendedOutputStream
import scalax.io.managed.OutputStreamResource
import java.io.OutputStream


class PrintableOutputStreamResource(out: OutputStream) extends OutputStreamResource[OutputStream](out) {
  def print(x: Any *)(implicit w: Writer): Unit = out.print(x: _*)(w)
  def printf(x: Any *)(implicit w: Writer): Unit = out.printf(x: _*)(w)
  def println(x: Any *)(implicit w: Writer): Unit = out.println(x: _*)(w)
}
