
package wok.process

import wok.core.Helpers.{EscapedString, QuotedString}


object Helpers {
  implicit def seq2process(seq: Seq[String]) = new Process(seq)

  implicit def result2out(r: Result) = r.out

  implicit class ProcessArgumentString(val s: String) extends AnyVal {
    def escaped: String = {
      new EscapedString(s).escaped('\\', '"').quoted('"')
    }
  }
}
