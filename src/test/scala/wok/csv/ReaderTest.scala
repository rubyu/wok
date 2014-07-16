
package wok.csv

import org.specs2.mutable._


class ReaderTest extends SpecificationWithJUnit {

  "Reader" should {
    "return Row" in {
      val result = new Reader()
        .FS("""\t""".r)
        .RS("""(\r\n|\r|\n)""".r)
        .FQ(QuoteOption() None())
        .open(new java.io.StringReader("a1\ta2\ta3\nb1\tb2\tb3"))
      result.toList mustEqual List(
        Row(0, List("a1", "a2", "a3"), List("\t", "\t"), "\n"),
        Row(1, List("b1", "b2", "b3"), List("\t", "\t"), ""))
    }
  }
}
