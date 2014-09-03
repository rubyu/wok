
package scalax.io

import wok.reflect.AbstractWok
import wok.reflect.Helpers.ExtendedOutputStream
import scalax.io.managed.OutputStreamResource
import java.io.OutputStream


class PrintableOutputStreamResource(out: OutputStream) extends OutputStreamResource[OutputStream](out) {
  def print(x: Any *)(implicit wok: AbstractWok): Unit = out.print(x: _*)(wok)
  def printf(x: Any *)(implicit wok: AbstractWok): Unit = out.printf(x: _*)(wok)
  def println(x: Any *)(implicit wok: AbstractWok): Unit = out.println(x: _*)(wok)
}
