package scalax.file.defaultfs

import scalax.io.StandardOpenOption
import wok.csv.Writer
import wok.reflect.Helpers.{AppendModePath, PrintableOutputStream}


class AppendModePath(defaultPath: DefaultPath) extends DefaultPath(defaultPath.jfile, defaultPath.fileSystem) {
  def print(x: Any *)(implicit w: Writer): Unit = defaultPath <<| { _.print(x: _*)(w) }
  def println(x: Any *)(implicit w: Writer): Unit = defaultPath <<| { _.println(x: _*)(w) }
}
