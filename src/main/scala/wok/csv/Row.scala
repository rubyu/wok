
package wok.csv


case class Row0(field: List[String], sep: List[String]) {
  def toRow1(term: String) = Row1(field, sep, term)
}
case class Row1(field: List[String], sep: List[String], term: String) {
  def toRow(id: Int) = Row(id, field, sep, term)
}
case class Row(val id: Int, val get: List[String], val sep: List[String], val term: String)
