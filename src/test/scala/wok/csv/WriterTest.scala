
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

    "with Quote().All().E(\\)" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\')).writeln(pout, "a")
        out.toString("utf-8") mustEqual "\"a\"\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\')).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "\"a\",\"b\"\n"
      }
      "write a String containing E" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\')).writeln(pout, "\\")
        out.toString("utf-8") mustEqual "\"\\\\\"\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\')).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\"\\\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\')).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All().E('\\')).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote().All()" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All()).writeln(pout, "a")
        out.toString("utf-8") mustEqual "\"a\"\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All()).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "\"a\",\"b\"\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All()).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\"\"\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All()).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().All()).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote().Min().E(\\)" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min().E('\\')).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min().E('\\')).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing E" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min().E('\\')).writeln(pout, "\\")
        out.toString("utf-8") mustEqual "\\\\\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min().E('\\')).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\\\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min().E('\\')).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min().E('\\')).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote().Min()" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min()).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min()).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing Q" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min()).writeln(pout, "\"")
        out.toString("utf-8") mustEqual "\"\"\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min()).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().Min()).writeln(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote().None().E(\\)" in {
      "write a String" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None().E('\\')).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None().E('\\')).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing E" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None().E('\\')).writeln(pout, "\\")
        out.toString("utf-8") mustEqual "\\\\\n"
      }
      "write a String containing OFS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None().E('\\')).writeln(pout, ",")
        out.toString("utf-8") mustEqual "\\,\n"
      }
      "throw an exception when a given string contains ORS" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None().E('\\')).writeln(pout, "\n") must throwA[RuntimeException]
      }
    }

    "with Quote().None()" in {
      "write a String with Quote().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None()).writeln(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq with Quote().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None()).writeln(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "throw an exception when a given string contains OFS with Quote().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None()).writeln(pout, ",") must throwA[RuntimeException]
      }
      "throw an exception when a given string contains ORS with Quote().None()" in new scope {
        new Writer().OFS(",").ORS("\n").OFQ(Quote().None()).writeln(pout, "\n") must throwA[RuntimeException]
      }
    }
  }
}