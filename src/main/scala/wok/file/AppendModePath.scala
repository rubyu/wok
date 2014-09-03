package scalax.file.defaultfs

import wok.reflect.AbstractWok
import wok.reflect.Helpers.{ExtendedPath, ExtendedOutputStream}


class AppendModePath(defaultPath: DefaultPath) extends DefaultPath(defaultPath.jfile, defaultPath.fileSystem) {
  def print(x: Any *)(implicit wok: AbstractWok): Unit = defaultPath #<< { _.print(x: _*)(wok) }
  def printf(x: Any *)(implicit wok: AbstractWok): Unit = defaultPath #<< { _.printf(x: _*)(wok) }
  def println(x: Any *)(implicit wok: AbstractWok): Unit = defaultPath #<< { _.println(x: _*)(wok) }
}
