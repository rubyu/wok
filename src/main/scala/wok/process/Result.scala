
package wok.process

import scalax.io.managed.InputStreamResource
import scalax.io.Resource
import java.io.InputStream


case class Result(
    val code: Int,
    _out: InputStream,
    _err: InputStream)
  extends InputStreamResource(_out) {
  def err = Resource.fromInputStream(_err)
}