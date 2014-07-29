package scalax.file.defaultfs

import scalax.io.StandardOpenOption
import wok.csv.Writer
import wok.reflect.Helpers.PrintableOutputStreamResource


class AppendModePath(defaultPath: DefaultPath) extends DefaultPath(defaultPath.jfile, defaultPath.fileSystem) {
  def print(x: Any *)(implicit w: Writer): Unit = outputStream(StandardOpenOption.Append).print(x: _*)(w)
  def println(x: Any *)(implicit w: Writer): Unit = outputStream(StandardOpenOption.Append).println(x: _*)(w)
}
