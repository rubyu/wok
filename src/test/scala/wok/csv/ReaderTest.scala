
package wok.csv

import org.specs2.mutable._
import java.io.ByteArrayInputStream

import scalax.io.Codec


class ReaderTest extends SpecificationWithJUnit {

  "Reader" should {
    "support shortcuts" in {
      "access without parens" in {
        Reader.FS.toString mustEqual """[ \t]+""".r.toString
        Reader.RS.toString mustEqual """(\r\n|\r|\n)""".r.toString
        Reader.FQ mustEqual Quote()
        Reader.CD mustEqual Codec.default
        Reader.FS("a").FS.toString mustEqual "a".r.toString
        Reader.RS("a").RS.toString mustEqual "a".r.toString
        Reader.FQ(Quote E('a')).FQ mustEqual Quote.E('a')
        Reader.CD(Codec.ISO8859).CD mustEqual Codec.ISO8859
        Reader.open(new ByteArrayInputStream("a".getBytes)).isInstanceOf[Iterator[_]] must beTrue
      }

      "'new' less constructor" in {
        Reader().isInstanceOf[Reader] must beTrue
      }

      "standard constructor" in {
        new Reader().isInstanceOf[Reader] must beTrue
      }
    }

    "return Row" in {
      Reader()
        .FS("""\t""".r)
        .RS("""(\r\n|\r|\n)""".r)
        .FQ(Quote() None())
        .open(new ByteArrayInputStream("a1\ta2\ta3\nb1\tb2\tb3".getBytes))
        .toList mustEqual List(
        Row(0, List("a1", "a2", "a3"), List("\t", "\t"), "\n", "a1\ta2\ta3\n"),
        Row(1, List("b1", "b2", "b3"), List("\t", "\t"), "", "b1\tb2\tb3"))
    }

    "have a Regex does not match to any string as FS when a empty string given" in {
      Reader()
        .FS("")
        .RS("""(\r\n|\r|\n)""".r)
        .FQ(Quote() None())
        .open(new ByteArrayInputStream("a1\ta2\nb1\tb2".getBytes))
        .toList mustEqual List(
        Row(0, List("a1\ta2"), Nil, "\n", "a1\ta2\n"),
        Row(1, List("b1\tb2"), Nil, "", "b1\tb2"))
    }

    "have a Regex does not match to any string as RS when a empty string given" in {
      Reader()
        .FS("""\t""".r)
        .RS("")
        .FQ(Quote() None())
        .open(new ByteArrayInputStream("a1\ta2\nb1\tb2".getBytes))
        .toList mustEqual List(
        Row(0, List("a1", "a2\nb1", "b2"), List("\t", "\t"), "", "a1\ta2\nb1\tb2"))
    }

    "be changed FS from inside Iterator" in {
      val reader = Reader()
        reader
        .FS("""\t""".r)
        .RS("\n")
        .FQ(Quote() None())
        .open(new ByteArrayInputStream("a1\ta2\nb1 b2".getBytes))
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
        .open(new ByteArrayInputStream("a\nb c".getBytes))
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
        .open(new ByteArrayInputStream("\"a\"\n\"b\"".getBytes))
        .map { row => reader.FQ(Quote() Min()); row }
        .toList mustEqual List(
        Row(0, List("\"a\""), Nil, "\n", "\"a\"\n"),
        Row(1, List("b"), Nil, "", "\"b\"")
      )
    }
  }
}
