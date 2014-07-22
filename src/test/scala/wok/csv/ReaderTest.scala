
package wok.csv

import org.specs2.mutable._


class ReaderTest extends SpecificationWithJUnit {

  "Reader" should {
    "close io.Reader" in {
      val in = new java.io.StringReader("")
      val result = Reader().open(in)
      result.toList mustEqual List()
      in.read() must throwAn[java.io.IOException]
    }

    "return Row" in {
      val result = Reader()
        .FS("""\t""".r)
        .RS("""(\r\n|\r|\n)""".r)
        .FQ(Quote() None())
        .open(new java.io.StringReader("a1\ta2\ta3\nb1\tb2\tb3"))
      result.toList mustEqual List(
        Row(0, List("a1", "a2", "a3"), List("\t", "\t"), "\n", "a1\ta2\ta3\n"),
        Row(1, List("b1", "b2", "b3"), List("\t", "\t"), "", "b1\tb2\tb3"))
    }

    "have a Regex does not match to any string as FS when a empty string given" in {
      val result = Reader()
        .FS("")
        .RS("""(\r\n|\r|\n)""".r)
        .FQ(Quote() None())
        .open(new java.io.StringReader("a1\ta2\nb1\tb2"))
      result.toList mustEqual List(
        Row(0, List("a1\ta2"), Nil, "\n", "a1\ta2\n"),
        Row(1, List("b1\tb2"), Nil, "", "b1\tb2"))
    }

    "have a Regex does not match to any string as RS when a empty string given" in {
      val result = Reader()
        .FS("""\t""".r)
        .RS("")
        .FQ(Quote() None())
        .open(new java.io.StringReader("a1\ta2\nb1\tb2"))
      result.toList mustEqual List(
        Row(0, List("a1", "a2\nb1", "b2"), List("\t", "\t"), "", "a1\ta2\nb1\tb2"))
    }

    "be changed FS from inside Iterator" in {
      val reader = Reader()
        reader
        .FS("""\t""".r)
        .RS("\n")
        .FQ(Quote() None())
        .open(new java.io.StringReader("a1\ta2\nb1 b2"))
        .map { row => reader.FS(" "); row }
        .toList mustEqual List(
          Row(0, List("a1", "a2"), Nil, "\n", "a1\ta2\n"),
          Row(1, List("b1", "b2"), Nil, "", "b1 b2")
        )
    }

    "be changed RS from inside Iterator" in {
      val reader = Reader()
      reader
        .FS("""\t""".r)
        .RS("\n")
        .FQ(Quote() None())
        .open(new java.io.StringReader("a\nb c"))
        .map { row => reader.RS(" "); row }
        .toList mustEqual List(
        Row(0, List("a"), Nil, "\n", "a\n"),
        Row(1, List("b"), Nil, " ", "b "),
        Row(2, List("c"), Nil, "", "c")
      )
    }

    "be changed FQ from inside Iterator" in {
      val reader = Reader()
      reader
        .FS("""\t""".r)
        .RS("\n")
        .FQ(Quote() None())
        .open(new java.io.StringReader("\"a\"\n\"b\""))
        .map { row => reader.FQ(Quote() Min()); row }
        .toList mustEqual List(
        Row(0, List("\"a\""), Nil, "\n", "\"a\"\n"),
        Row(1, List("b"), Nil, "", "\"b\"")
      )
    }
  }
}
