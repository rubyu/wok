
package wok.csv

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import wok.core.Helpers._


class Writer(OFS: String, ORS: String, OFQ: QuoteOption) {
  private lazy val escape: String => String = {
    OFQ match {
      //escapes e and q in a given string with e and quotes it with q
      case QuoteOption(QuoteAll, Some(q), Some(e)) => { _.escaped(e, q).quoted(q) }

      //quotes a given string with q
      case QuoteOption(QuoteAll, Some(q), None) => { _.escaped(q).quoted(q) }

      //escapes e and q in a given string with e and quotes it with q when it contains OFS or ORS
      case QuoteOption(QuoteMin, Some(q), Some(e)) => {
        case s if s.contains(OFS) || s.contains(ORS) => s.escaped(e, q).quoted(q)
        case s => s.escaped(e, q)
      }

      //escapes q in a given string with q and quotes it with q when it contains OFS or ORS or q
      case QuoteOption(QuoteMin, Some(q), None) => {
        case s if s.contains(OFS) || s.contains(ORS) || s.contains(q) => s.escaped(q).quoted(q)
        case s => s
      }

      //escapes OFS in a given string with e and throws an error when it contains ORS
      case QuoteOption(QuoteNone, _, Some(e)) => {
        case s if s.contains(ORS) => throw new RuntimeException
        case s => s.escaped(e, OFS)
      }

      //throws an error when a given string contains OFS or ORS
      case QuoteOption(QuoteNone, _, None) => {
        case s if s.contains(OFS) || s.contains(ORS) => throw new RuntimeException
        case s => s
      }
    }
  }

  //todo support codec
  def write(out: PrintStream, x: Any) {
    val str = x match {
      case x: Seq[_] => x.map { x => escape(x.toString) } mkString(OFS)
      case x => escape(x.toString)
    }
    out.write(str.getBytes(StandardCharsets.UTF_8))
    out.write(ORS.getBytes(StandardCharsets.UTF_8))
  }
}