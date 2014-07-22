
package wok.core

import scala.util.DynamicVariable
import java.io.InputStream


object SystemInput {
  private val inVar = new DynamicVariable[InputStream](java.lang.System.in)
  def get = inVar.value
  def withValue[T](in: InputStream)(thunk: =>T): T = inVar.withValue(in)(thunk)
}
