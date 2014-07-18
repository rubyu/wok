
package wok.core

import util.matching.Regex
import scalax.file.Path
import wok.csv.{Row, Writer}
import scalax.io.Resource


object Helpers {
  implicit class EscapedRegexString(val s: String) extends AnyVal {
    def er: Regex = new Regex(s.replaceAll("""(\\|\*|\+|\.|\?|\{|\}|\(|\)|\[|\]|\^|\$|\-|\|)""", """\\$1"""))
  }

  implicit class EscapedRegexChar(val c: Char) extends AnyVal {
    def er: Regex = c.toString.er
  }

  implicit class QuotedString(val str: String) extends AnyVal {
    def quoted(q: Char): String = q.toString + str + q.toString
  }

  implicit class EscapedString(val str: String) extends AnyVal {
    def escaped(e: Char): String = str.escaped(e.toString)
    def escaped(e: Char, t: Char): String = str.escaped(e.toString, t.toString)
    def escaped(e: Char, t: String): String = str.escaped(e.toString, t)
    def escaped(e: String, t: String): String = str.escaped(e).replace(t, e + t)
    def escaped(e: String): String = str.replace(e, e + e)
  }

  implicit class PrintablePath(val p: Path) extends AnyVal {
    def print(x: Any)(implicit w: Writer): Unit =
      Resource.fromFile(p.fileOption.get).outputStream.acquireFor { w.write(_, x) }

    def println(x: Any = "")(implicit w: Writer): Unit =
      Resource.fromFile(p.fileOption.get).outputStream.acquireFor { w.writeln(_, x) }
  }

  implicit class CompletableUnit(val u: Any) extends AnyVal {
    def complete(): Unit = {}
  }

  implicit class CompletableIterator(val itr: Iterator[_]) extends AnyVal {
    def complete(): Unit = itr.foreach { x => }
  }
}
