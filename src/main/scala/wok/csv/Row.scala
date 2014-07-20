
package wok.csv


case class Row0(field: List[String], sep: List[String]) {
  def toRow1(term: String) = Row1(field, sep, term)
}
case class Row1(field: List[String], sep: List[String], term: String) {
  def toRow(id: Long, source: String) = Row(id, field, sep, term, source)
}

class Row(val id: Long, field: List[String], val sep: List[String], val term: String,
          val source: String) extends Seq[String] {
  def length = field.length
  def apply(idx: Int) = field(idx)
  def iterator = field.iterator
}

object Row {
  def apply(id: Long, field: List[String], sep: List[String], term: String, source: String) =
    new Row(id, field, sep, term, source)
}
