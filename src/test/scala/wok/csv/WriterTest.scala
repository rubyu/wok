
package wok.csv

import org.specs2.mutable._
import org.specs2.specification.Scope
import java.nio.charset.StandardCharsets
import wok.Helpers._

import scalax.io.Codec


class WriterTest extends SpecificationWithJUnit {

  class scope extends Scope {
    val out = new TestOutputStream
    def result = new String(out.toByteArray, StandardCharsets.UTF_8)
  }

  "Writer" should {
    "support shortcuts" in {
      "access without parens" in {
        Writer.OFS mustEqual " "
        Writer.ORS mustEqual "\n"
        Writer.OFQ mustEqual Quote.None()
        Writer.OCD mustEqual Codec.default
        Writer.OFS("a").OFS mustEqual "a"
        Writer.ORS("a").ORS mustEqual "a"
        Writer.OFS('a').OFS mustEqual "a"
        Writer.ORS('a').ORS mustEqual "a"
        Writer.OFQ(Quote E('a')).OFQ mustEqual Quote.E('a')
        Writer.OCD(Codec.ISO8859).OCD mustEqual Codec.ISO8859

        "write String" in new scope {
          Writer.write(out, "a")
          result mustEqual "a"
        }

        "writeField String" in new scope {
          Writer.writeField(out, "a")
          result mustEqual "a "
        }

        "writeRow String" in new scope {
          Writer.writeRow(out, "a")
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
      
      val writer = Writer OFS(",") ORS("\n") OFQ(Quote All() E('\\'))
      
      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "\"a\""
      }
      "write E" in new scope {
        writer.write(out, "\\")
        result mustEqual "\"\\\"" //compatible with Python
      }
      "write Q" in new scope {
        writer.write(out, "\"")
        result mustEqual "\"\"\"\"" //compatible with Python
      }
      "write OFS" in new scope {
        writer.write(out, ",")
        result mustEqual "\",\""
      }
      "write ORS" in new scope {
        writer.write(out, "\n")
        result mustEqual "\"\n\""
      }
    }

    "with QuoteAll Q" in {
      
      val writer = Writer OFS(",") ORS("\n") OFQ(Quote All() Q('"'))
      
      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "\"a\""
      }
      "write Q" in new scope {
        writer.write(out, "\"")
        result mustEqual "\"\"\"\""
      }
      "write OFS" in new scope {
        writer.write(out, ",")
        result mustEqual "\",\""
      }
      "write ORS" in new scope {
        writer.write(out, "\n")
        result mustEqual "\"\n\""
      }
    }

    "with QuoteMin Q E" in {

      val writer = Writer OFS(",") ORS("\n") OFQ(Quote Min() Q('"') E('\\'))

      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "a"
      }
      "write Q" in new scope {
        writer.write(out, "\"")
        result mustEqual "\"\"\"\"" //compatible with Python
      }
      "write E" in new scope {
        writer.write(out, "\\")
        result mustEqual "\"\\\"" //compatible with Python
      }
      "write OFS" in new scope {
        writer.write(out, ",")
        result mustEqual "\",\""
      }
      "write ORS" in new scope {
        writer.write(out, "\n")
        result mustEqual "\"\n\""
      }
    }

    "with QuoteMin Q" in {
      
      val writer = Writer OFS(",") ORS("\n") OFQ(Quote Min() Q('"'))
      
      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "a"
      }
      "write Q" in new scope {
        writer.write(out, "\"")
        result mustEqual "\"\"\"\""
      }
      "write OFS" in new scope {
        writer.write(out, ",")
        result mustEqual "\",\""
      }
      "write ORS" in new scope {
        writer.write(out, "\n")
        result mustEqual "\"\n\""
      }
    }

    "with QuoteNone Q E" in {

      val writer = Writer OFS(",") ORS("\n") OFQ(Quote None() Q('"') E('\\'))

      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "a"
      }
      "write Q" in new scope {
        writer.write(out, "\"")
        result mustEqual "\\\"" //compatible with Python
      }
      "write E" in new scope {
        writer.write(out, "\\")
        result mustEqual "\\\\"
      }
      "write OFS" in new scope {
        writer.write(out, ",")
        result mustEqual "\\,"
      }
      "write ORS" in new scope {
        writer.write(out, "\n")
        result mustEqual "\\\n"
      }
    }

    "with QuoteNone E" in {
      
      val writer = Writer OFS(",") ORS("\n") OFQ(Quote None() E('\\'))
      
      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "a"
      }
      "write E" in new scope {
        writer.write(out, "\\")
        result mustEqual "\\\\"
      }
      "write OFS" in new scope {
        writer.write(out, ",")
        result mustEqual "\\,"
      }
      "write ORS" in new scope {
        writer.write(out, "\n")
        result mustEqual "\\\n"
      }
    }

    "with QuoteNone Q" in {

      val writer = Writer OFS(",") ORS("\n") OFQ(Quote None() Q('"'))

      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "a"
      }
      "throw EncodingException when given string contains Q" in new scope {
        writer.write(out, "\"") must throwA[EncodingException] //compatible with Python
      }
      "throw EncodingException when given string contains OFS" in new scope {
        writer.write(out, ",") must throwA[EncodingException]
      }
      "throw EncodingException when given string contains ORS" in new scope {
        writer.write(out, "\n") must throwA[EncodingException]
      }
    }

    "with QuoteNone" in {

      val writer = Writer OFS(",") ORS("\n") OFQ(Quote None())

      "write String" in new scope {
        writer.write(out, "a")
        result mustEqual "a"
      }
      "throw EncodingException when given string contains OFS" in new scope {
        writer.write(out, ",") must throwA[EncodingException]
      }
      "throw EncodingException when given string contains ORS" in new scope {
        writer.write(out, "\n") must throwA[EncodingException]
      }
    }
  }

  "Writer.equals" should {
    "return true" in {
      Writer() mustEqual Writer()
      Writer.OFS("a") mustEqual Writer.OFS("a")
      Writer.ORS("a") mustEqual Writer.ORS("a")
      Writer.OFQ(Quote.All()) mustEqual Writer.OFQ(Quote.All())
      Writer.OCD(Codec("Windows-31J")) mustEqual Writer.OCD(Codec("Windows-31J"))
    }
  }

  "Writer.copy" should {
    "return copied instances" in {
      val w = Writer()
      w == w.copy() must beTrue
      w eq w.copy() must beFalse
    }
    "set values" in {
      Writer.OFS("a") mustEqual Writer().copy(OFS = "a")
      Writer.ORS("a") mustEqual Writer().copy(ORS = "a")
      Writer.OFQ(Quote.All()) mustEqual Writer().copy(OFQ = Quote.All())
      Writer.OCD(Codec("Windows-31J")) mustEqual Writer().copy(OCD = Codec("Windows-31J"))
    }
  }
}