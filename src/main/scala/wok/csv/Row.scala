
package wok.csv


case class Row0(field: List[String], sep: List[String]) {
  def toRow1(term: String) = Row1(field, sep, term)
}
case class Row1(field: List[String], sep: List[String], term: String) {
  def toRow(id: Long) = Row(id, field, sep, term)
}
case class Row(id: Long, field: List[String], sep: List[String], term: String)
