
package wok.core

import java.io.{InputStream, OutputStream}

import scala.util.DynamicVariable
import scalax.io.managed.{InputStreamResource, OutputStreamResource}
import scalax.io.Resource


object Stdio {
  private val outVar = new DynamicVariable[OutputStreamResource[OutputStream]](Resource.fromOutputStream(java.lang.System.out))
  private val errVar = new DynamicVariable[OutputStreamResource[OutputStream]](Resource.fromOutputStream(java.lang.System.err))
  private val inVar = new DynamicVariable[InputStreamResource[InputStream]](Resource.fromInputStream(java.lang.System.in))

  def out = outVar.value
  def err = errVar.value
  def in = inVar.value

  def withOut[T](out: OutputStream)(thunk: =>T): T = outVar.withValue(Resource.fromOutputStream(out))(thunk)
  def withErr[T](err: OutputStream)(thunk: =>T): T = errVar.withValue(Resource.fromOutputStream(err))(thunk)
  def withIn[T](in: InputStream)(thunk: =>T): T = inVar.withValue(Resource.fromInputStream(in))(thunk)
}
