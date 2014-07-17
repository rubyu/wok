
package wok.process

import scalax.io.managed.InputStreamResource
import java.io.InputStream


case class Result(code: Int, out: InputStreamResource[InputStream], err: InputStreamResource[InputStream])