
package wok.csv


case class Row0(field: List[String], sep: List[String]) {
  def toRow1(term: String) = Row1(field, sep, term)
}
case class Row1(field: List[String], sep: List[String], term: String) {
  def toRow(reader: Reader, id: Long, source: String) = new Row(reader, id, field, sep, term, source)
}

class Row(val reader: Reader, val id: Long, field: List[String], val sep: List[String], val term: String,
          val source: String) extends Seq[String] {
  def length = field.length
  def apply(idx: Int) = field(idx)
  def iterator = field.iterator
}
