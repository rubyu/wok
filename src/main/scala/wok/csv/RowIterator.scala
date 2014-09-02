
package wok.csv

import annotation.tailrec
import java.io


class RowIterator(in: io.Reader, parser: Reader) extends Iterator[Row] {
  private def read(until: Int): CharSequence = {
    val buf = new Array[Char](until)
    in.read(buf) match {
      case -1 => ""
      case n => new FastCharSequence(buf, 0, n)
    }
  }

  private var resultId = -1

  private var buffer: CharSequence = ""
  private var reachEnd = false

  private def parseNext(): Option[Row] = {
    @tailrec
    def _parseNext(canBeLast: Boolean = false): Option[Row] = {
      buffer.length match {
        case 0 if reachEnd => None
        case _ =>
          parser.parse(buffer) match {
            /*
            Parser possibly returns false-positive Success() when Reader hasn't reach to the in's end and
              result.next.size is zero.

            | Buffer  | Rest |
            |---------|------|
            | "a,b,c" | ""   | => Success
            | "a,b"   | ",c" | => false-positive Success
            */
            case x if x.successful && (reachEnd || !x.next.atEnd) =>
              resultId += 1
              try Some(x.get.toRow(resultId))
              finally buffer = buffer.subSequence(x.next.offset, buffer.length)
            case x if canBeLast =>
              throw new DecodingException(s"""parse failed at line ${resultId + 2}, after '${buffer.subSequence(0, Math.max(buffer.length, 100))}'""")
            case x =>
              if (!reachEnd) {
                reachEnd = read(math.max(1000000, buffer.length)) match {
                  case s if s.length == 0 => true
                  case s if buffer.length == 0 => buffer = s; false
                  case s => buffer = new JointCharSequence(buffer, s); false
                }
              }
              _parseNext(reachEnd)
          }
      }
    }
    _parseNext()
  }

  private var _next: Option[Row] = None

  def hasNext = {
    _next match {
      case Some(x) => true
      case None => _next = parseNext(); _next.isDefined
    }
  }

  def next() = {
    _next match {
      case Some(x) => _next = None; x
      case None => parseNext().getOrElse(throw new NoSuchElementException)
    }
  }
}