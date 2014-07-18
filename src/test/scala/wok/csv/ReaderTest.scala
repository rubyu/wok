
package wok.csv

import org.specs2.mutable._


class ReaderTest extends SpecificationWithJUnit {

  "Reader" should {
    "close io.Reader" in {
      val in = new java.io.StringReader("")
      val result = new Reader().open(in)
      result.toList mustEqual List()
      in.read() must throwAn[java.io.IOException]
    }

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

    "have a Regex does not match to any string as FS when a empty string given" in {
      val result = new Reader()
        .FS("")
        .RS("""(\r\n|\r|\n)""".r)
        .FQ(QuoteOption() None())
        .open(new java.io.StringReader("a1\ta2\nb1\tb2"))
      result.toList mustEqual List(
        Row(0, List("a1\ta2"), Nil, "\n"),
        Row(1, List("b1\tb2"), Nil, ""))
    }

    "have a Regex does not match to any string as RS when a empty string given" in {
      val result = new Reader()
        .FS("""\t""".r)
        .RS("")
        .FQ(QuoteOption() None())
        .open(new java.io.StringReader("a1\ta2\nb1\tb2"))
      result.toList mustEqual List(
        Row(0, List("a1", "a2\nb1", "b2"), List("\t", "\t"), ""))
    }
  }
}
