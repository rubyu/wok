package scalax.file.defaultfs

import wok.reflect.AbstractWok


class RedirectModePath(defaultPath: DefaultPath) extends AppendModePath(defaultPath) {

  private var firstOp = true

  override def print(x: Any *)(implicit wok: AbstractWok): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.print(x: _*)(wok)
  }

  override def printf(x: Any *)(implicit wok: AbstractWok): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.printf(x: _*)(wok)
  }

  override def println(x: Any *)(implicit wok: AbstractWok): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.println(x: _*)(wok)
  }
}
