package scalax.file.defaultfs

import wok.core.PrintableElement
import wok.csv.Writer


class RedirectModePath(defaultPath: DefaultPath) extends AppendModePath(defaultPath) {

  private var firstOp = true

  override def print(x: PrintableElement *)(implicit w: Writer): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.print(x: _*)(w)
  }

  override def println(x: PrintableElement *)(implicit w: Writer): Unit = {
    if (firstOp) {
      truncate(0)
      firstOp = false
    }
    super.println(x: _*)(w)
  }
}
