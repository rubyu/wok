
package wok.csv

import util.matching.Regex
import util.parsing.combinator.RegexParsers
import wok.core.Helpers._


class Parser(FS: Regex, RS: Regex, FQ: Quote) extends RegexParsers {
  override val skipWhitespace = false

  /*
  # RFC4180
  * Each record is located on a separate line, delimited by a line break (CRLF).
  * The last record in the file may or may not have an ending line break.

  # Wok
  ## Relaxations
  * Records may be delimited by strings other than CRLF.
   */
  lazy val line: Parser[Row1] =
    row_empty | row ~ (RS | EOF) ^^
      { case row0 ~ term => row0.toRow1(term) }

  lazy val row_empty: Parser[Row1] =
    (RS | EOF) ^^
      { Row1(Nil, Nil, _) }

  /*
  # RFC4180
  * Within the header and each record, there may be one or more fields, separated by commas.
  * Each line should contain the same number of fields throughout the file.
  * Spaces are considered part of a field and should not be ignored.
  * The last field in the record must not be followed by a comma.

  # Wok
  ## Relaxations
  * Records may be empty.
  * Fields may be separated by strings other than commas.
  * Records may contain the variant number of fields.
   */
  lazy val row: Parser[Row0] =
    field ~ ( rep( FS ~ field ) ).? ^^
      {
        case first ~ Some(rest) => rest.map { case fs ~ f => (fs, f) }.unzip match { case (fs, f) => Row0(first +: f, fs) }
        case first ~ None       => Row0(List(first), Nil)
      }

  /*
  # RFC4180
  * Each field may or may not be enclosed in double quotes (however some programs, such as Microsoft Excel, do not use
      double quotes at all).
  * If fields are not enclosed with double quotes, then double quotes may not appear inside the fields.
  * Fields containing line breaks (CRLF), double quotes, and commas should be enclosed in double-quotes.
  * If double-quotes are used to enclose fields, then a double-quote appearing inside a field must be escaped by
      preceding it with another double quote.

  # Wok
  ## Specifications
  * Escape characters escape quote-characters, field-separators, line-separators and itself.
  * Fields enclosed with quote-characters and not containing any quote-character excluding those escaped by another
      quote-character or a escape-character are treated as quoted fields, otherwise the fields are treated as
      non-quoted fields.

  ## Relaxations
  * Characters other than double quote may be used as quote character.
  * Quote-characters, field-separators and line-separators, following right after escape-characters, may appear inside
      the fields both enclosed and not enclosed with quote-characters.
  * Non-quoted fields may contain quote-characters.
   */
  lazy val field : Parser[String] = FQ match {
    case Quote(Quote.Mode.All, Some(q), Some(e)) => quoted( q, text(q, e) )
    case Quote(Quote.Mode.All, Some(q),    None) => quoted( q, text(q) )
    case Quote(Quote.Mode.Min, Some(q), Some(e)) => quoted( q, text(q, e) ) | non_quoted(e)
    case Quote(Quote.Mode.Min, Some(q),    None) => quoted( q, text(q) )    | non_quoted
    case Quote(Quote.Mode.None,      _, Some(e)) => non_quoted(e)
    case Quote(Quote.Mode.None,      _,    None) => non_quoted
  }

  /*
  Strings enclosed with quote-characters.
    */
  def quoted(Q: Char, T: Parser[String]): Parser[String] =
    Q ~> T <~ Q

  /*
  Quote-characters escaped with another quote-character or
    strings consist of characters other than quote-character.
  Larger and not equal to zero.
   */
  def text(Q: Char): Parser[String] =
    rep( Q ~> Q | s"""[^${Q.er}]+""".r ) ^^ { _.mkString }

  /*
  Quote-characters escaped with another quote-character or
    escape-characters, field-separators and line-separators escaped with escape-character or
    strings consist of characters other than quote-character and escape-character.
  Larger and not equal to zero.
   */
  def text(Q: Char, E: Char): Parser[String] =
    rep( Q ~> Q | E ~> s"""(${Q.er}|${E.er}|$FS|$RS)""".r | E ^^^ "" | s"""[^${Q.er}${E.er}]+""".r ) ^^ { _.mkString }

  /*
  Escape-characters, field-separators and line-separators escaped with escape-character or
    escape-characters or
    strings consist of strings other than field-separators and line-separators and
                       characters other than quote-character and escape-character.
  Larger and not equal to zero.
   */
  def non_quoted(E: Char): Parser[String] =
    rep( E ~> s"""(${E.er}|$FS|$RS)""".r | E ^^^ "" | s"""((?!$FS)(?!$RS)[^${E.er}])+""".r ) ^^ { _.mkString }

  /*
  Strings consist of strings other than field-separators and line-separators.
  Larger than zero.
   */
  def non_quoted: Parser[String] =
    s"""((?!$FS)(?!$RS)(?s).)*""".r

  def EOF: Regex = """\z""".r

  def parse(in: CharSequence): ParseResult[Row1] = parse(line, in)
}
