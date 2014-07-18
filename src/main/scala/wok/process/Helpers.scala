
package wok.process


object Helpers {
  implicit def seq2process(seq: Seq[String]) = new Process(seq)

  implicit def result2out(r: Result) = r.out
}
