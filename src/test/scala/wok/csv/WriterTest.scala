
package wok.csv

import java.nio.charset.StandardCharsets

import org.specs2.mutable._
import org.specs2.specification.Scope
import java.io.ByteArrayOutputStream


class WriterTest extends SpecificationWithJUnit {

  class scope extends Scope {
    val out = new ByteArrayOutputStream
    def result = new String(out.toByteArray, StandardCharsets.UTF_8)
  }

  "Writer" should {

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