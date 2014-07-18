
package wok.csv

import org.specs2.mutable._
import org.specs2.specification.Scope
import java.io.{PrintStream, ByteArrayOutputStream}


class WriterTest extends SpecificationWithJUnit {

  class scope extends Scope {
    val out = new ByteArrayOutputStream
    val pout = new PrintStream(out)
  }

  "Writer" should {

    "with QuoteOption().All().E(\\)" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All().E('\\')).writeln(pout, "a")
        out.toString("utf-8") mustEqual "\"a\"\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All().E('\\')).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "\"a\",\"b\"\n"
      }
      "write a String containing E" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All().E('\\')).writeln(pout, "\\")
        out.toString("utf-8") mustEqual "\"\\\\\"\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All().E('\\')).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\"\\\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All().E('\\')).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All().E('\\')).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with QuoteOption().All()" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All()).writeln(pout, "a")
        out.toString("utf-8") mustEqual "\"a\"\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All()).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "\"a\",\"b\"\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All()).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\"\"\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All()).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().All()).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with QuoteOption().Min().E(\\)" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min().E('\\')).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min().E('\\')).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing E" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min().E('\\')).writeln(pout, "\\")
        out.toString("utf-8") mustEqual "\\\\\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min().E('\\')).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\\\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min().E('\\')).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min().E('\\')).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with QuoteOption().Min()" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min()).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min()).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min()).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\"\"\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min()).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().Min()).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with QuoteOption().None().E(\\)" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None().E('\\')).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None().E('\\')).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing E" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None().E('\\')).writeln(pout, "\\")
        out.toString("utf-8") mustEqual "\\\\\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None().E('\\')).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\\,\n"
      }
      "throw an exception when a given string contains ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None().E('\\')).writeln(pout, "\n") must throwA[RuntimeException]
      }
    }

    "with QuoteOption().None()" in {
      "write a String with QuoteOption().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None()).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq with QuoteOption().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None()).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "throw an exception when a given string contains OFS with QuoteOption().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None()).writeln(pout, ",") must throwA[RuntimeException]
      }
      "throw an exception when a given string contains ORS with QuoteOption().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(QuoteOption().None()).writeln(pout, "\n") must throwA[RuntimeException]
      }
    }
  }
}