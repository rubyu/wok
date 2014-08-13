package scalax.file.defaultfs

import wok.csv.Writer


class RedirectModePath(defaultPath: DefaultPath) extends AppendModePath(defaultPath) {

  private var firstOp = true

  override def print(x: Any *)(implicit w: Writer): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.print(x: _*)(w)
  }

  override def printf(x: Any *)(implicit w: Writer): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.printf(x: _*)(w)
  }

  override def println(x: Any *)(implicit w: Writer): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.println(x: _*)(w)
  }
}
