
package wok.reflect

import scala.tools.nsc.reporters.StoreReporter


class Report(source: String, reporter: StoreReporter) {

  private val messages = reporter.infos.filter( i => i.severity ==  reporter.ERROR || i.severity == reporter.WARNING ).toList

  private def errors = messages.count(i => i.severity == reporter.ERROR)
  private def warning = messages.count(i => i.severity == reporter.WARNING)

  def hasError = errors > 0

  override def toString = {
    val ls = System.lineSeparator()
    val b = new StringBuilder
    if (messages.nonEmpty) {
      b.append(s"Errors: $errors, Warning: $warning")
      b.append(ls * 2)
      messages foreach { info =>
        b.append(
          List(
            s"${info.pos.line}: ${info.severity}: ${info.msg}",
            info.pos.lineContent.stripLineEnd,
            " " * (info.pos.column - 1) + "^"
          ) mkString(ls)
        )
      }
      b.append(ls * 2)
    }
    else {
      b.append("No errors and warning found.")
      b.append(ls * 2)
    }

    def sourceWithLineNumber = {
      val template = s"|%${source.lines.size.toString.size}d| %s"
      source.lines.zipWithIndex.map { case (line, idx) =>
        template.format(idx+1, line)
      } mkString(ls)
    }

    b.append(sourceWithLineNumber)
    b.result()
  }
}