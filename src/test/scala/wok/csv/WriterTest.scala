
package wok.csv

import org.specs2.mutable._
import org.specs2.specification.Scope
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scalax.io.Codec


class WriterTest extends SpecificationWithJUnit {

  class scope extends Scope {
    val out = new ByteArrayOutputStream
    def result = new String(out.toByteArray, StandardCharsets.UTF_8)
  }

  "Writer" should {
    "support shortcuts" in {
      "access without parens" in {
        Writer.OFS mustEqual " "
        Writer.ORS mustEqual "\n"
        Writer.OFQ mustEqual Quote()
        Writer.OCD mustEqual Codec.default
        Writer.OFS("a").OFS mustEqual "a"
        Writer.ORS("a").ORS mustEqual "a"
        Writer.OFQ(Quote E('a')).OFQ mustEqual Quote.E('a')
        Writer.OCD(Codec.ISO8859).OCD mustEqual Codec.ISO8859

        "write String" in new scope {
          Writer.write(out, "a")
          result mustEqual "a"
        }

        "write Seq" in new scope {
          Writer.write(out, Seq("a"))
          result mustEqual "a"
        }

        "writeln String" in new scope {
          Writer.writeln(out, "a")
          result mustEqual "a\n"
        }

        "writeln Seq" in new scope {
          Writer.writeln(out, Seq("a"))
          result mustEqual "a\n"
        }
      }

      "'new' less constructor" in {
        Writer().isInstanceOf[Writer] must beTrue
      }

      "standard constructor" in {
        new Writer().isInstanceOf[Writer] must beTrue
      }
    }

    "with QuoteAll Q E" in {
      
      val writer = Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\'))
      
      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "\"a\"\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "\"a\",\"b\"\n"
      }
      "write E" in new scope {
        writer.writeln(out, "\\")
        result mustEqual "\"\\\"\n" //compatible with Python
      }
      "write Q" in new scope {
        writer.writeln(out, "\"")
        result mustEqual "\"\"\"\"\n" //compatible with Python
      }
      "write OFS" in new scope {
        writer.writeln(out, ",")
        result mustEqual "\",\"\n"
      }
      "write ORS" in new scope {
        writer.writeln(out, "\n")
        result mustEqual "\"\n\"\n"
      }
    }

    "with QuoteAll Q" in {
      
      val writer = Writer().OFS(",").ORS("\n").OFQ(Quote() All() Q('"'))
      
      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "\"a\"\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "\"a\",\"b\"\n"
      }
      "write Q" in new scope {
        writer.writeln(out, "\"")
        result mustEqual "\"\"\"\"\n"
      }
      "write OFS" in new scope {
        writer.writeln(out, ",")
        result mustEqual "\",\"\n"
      }
      "write ORS" in new scope {
        writer.writeln(out, "\n")
        result mustEqual "\"\n\"\n"
      }
    }

    "with QuoteMin Q E" in {

      val writer = Writer() OFS(",") ORS("\n") OFQ(Quote() Min() Q('"') E('\\'))

      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "a\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "a,b\n"
      }
      "write Q" in new scope {
        writer.writeln(out, "\"")
        result mustEqual "\"\"\"\"\n" //compatible with Python
      }
      "write E" in new scope {
        writer.writeln(out, "\\")
        result mustEqual "\"\\\"\n" //compatible with Python
      }
      "write OFS" in new scope {
        writer.writeln(out, ",")
        result mustEqual "\",\"\n"
      }
      "write ORS" in new scope {
        writer.writeln(out, "\n")
        result mustEqual "\"\n\"\n"
      }
    }

    "with QuoteMin Q" in {
      
      val writer = Writer() OFS(",") ORS("\n") OFQ(Quote() Min() Q('"'))
      
      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "a\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "a,b\n"
      }
      "write Q" in new scope {
        writer.writeln(out, "\"")
        result mustEqual "\"\"\"\"\n"
      }
      "write OFS" in new scope {
        writer.writeln(out, ",")
        result mustEqual "\",\"\n"
      }
      "write ORS" in new scope {
        writer.writeln(out, "\n")
        result mustEqual "\"\n\"\n"
      }
    }

    "with QuoteNone Q E" in {

      val writer = Writer() OFS(",") ORS("\n") OFQ(Quote() None() Q('"') E('\\'))

      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "a\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "a,b\n"
      }
      "write Q" in new scope {
        writer.writeln(out, "\"")
        result mustEqual "\\\"\n" //compatible with Python
      }
      "write E" in new scope {
        writer.writeln(out, "\\")
        result mustEqual "\\\\\n"
      }
      "write OFS" in new scope {
        writer.writeln(out, ",")
        result mustEqual "\\,\n"
      }
      "write ORS" in new scope {
        writer.writeln(out, "\n")
        result mustEqual "\\\n\n"
      }
    }

    "with QuoteNone E" in {
      
      val writer = Writer() OFS(",") ORS("\n") OFQ(Quote() None() E('\\'))
      
      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "a\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "a,b\n"
      }
      "write E" in new scope {
        writer.writeln(out, "\\")
        result mustEqual "\\\\\n"
      }
      "write OFS" in new scope {
        writer.writeln(out, ",")
        result mustEqual "\\,\n"
      }
      "write ORS" in new scope {
        writer.writeln(out, "\n")
        result mustEqual "\\\n\n"
      }
    }

    "with QuoteNone Q" in {

      val writer = Writer() OFS(",") ORS("\n") OFQ(Quote() None() Q('"'))

      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "a\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "a,b\n"
      }
      "throw EncodingException when given string contains Q" in new scope {
        writer.writeln(out, "\"") must throwA[EncodingException] //compatible with Python
      }
      "throw EncodingException when given string contains OFS" in new scope {
        writer.writeln(out, ",") must throwA[EncodingException]
      }
      "throw EncodingException when given string contains ORS" in new scope {
        writer.writeln(out, "\n") must throwA[EncodingException]
      }
    }

    "with QuoteNone" in {

      val writer = Writer() OFS(",") ORS("\n") OFQ(Quote() None())

      "write String" in new scope {
        writer.writeln(out, "a")
        result mustEqual "a\n"
      }
      "write Seq" in new scope {
        writer.writeln(out, Seq("a", "b"))
        result mustEqual "a,b\n"
      }
      "throw EncodingException when given string contains OFS" in new scope {
        writer.writeln(out, ",") must throwA[EncodingException]
      }
      "throw EncodingException when given string contains ORS" in new scope {
        writer.writeln(out, "\n") must throwA[EncodingException]
      }
    }
  }
}