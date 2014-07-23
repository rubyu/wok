
package wok.process


object Helpers {
  implicit def seq2process(seq: Seq[_]) = new Proc(seq map { _.toString })

  implicit def result2out(r: Result) = r.out
}
