
package wok.core

import java.io.{InputStream, OutputStream}

import scala.util.DynamicVariable
import scalax.io.managed.{InputStreamResource, OutputStreamResource}
import scalax.io.Resource


object Stdio {
  private object CloseGuard {
    def apply(in: InputStream) = new CloseGuardInputStream(in)
    def apply(out: OutputStream) = new CloseGuardOutputStream(out)

    class CloseGuardInputStream(in: InputStream) extends InputStream {
      def read(): Int = in.read()
    }
    class CloseGuardOutputStream(out: OutputStream) extends OutputStream {
      def write(b: Int) = out.write(b)
      override def flush() = out.flush()
    }
  }

  private val outVar =
    new DynamicVariable[OutputStreamResource[OutputStream]](
      Resource.fromOutputStream(CloseGuard(java.lang.System.out)))
  private val errVar =
    new DynamicVariable[OutputStreamResource[OutputStream]](
      Resource.fromOutputStream(CloseGuard(java.lang.System.err)))
  private val inVar =
    new DynamicVariable[InputStreamResource[InputStream]](
      Resource.fromInputStream(CloseGuard(java.lang.System.in)))

  def out = outVar.value
  def err = errVar.value
  def in = inVar.value

  def withOut[T](out: OutputStream)(thunk: =>T): T =
    outVar.withValue(Resource.fromOutputStream(CloseGuard(out)))(thunk)
  def withErr[T](err: OutputStream)(thunk: =>T): T =
    errVar.withValue(Resource.fromOutputStream(CloseGuard(err)))(thunk)
  def withIn[T](in: InputStream)(thunk: =>T): T =
    inVar.withValue(Resource.fromInputStream(CloseGuard(in)))(thunk)
}
