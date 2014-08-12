package scalax.file.defaultfs

import wok.core.PrintableElement
import wok.csv.Writer
import wok.reflect.Helpers.{ExtendedPath, ExtendedOutputStream}


class AppendModePath(defaultPath: DefaultPath) extends DefaultPath(defaultPath.jfile, defaultPath.fileSystem) {
  def print(x: PrintableElement *)(implicit w: Writer): Unit = defaultPath #<< { _.print(x: _*)(w) }
  def println(x: PrintableElement *)(implicit w: Writer): Unit = defaultPath #<< { _.println(x: _*)(w) }
}
