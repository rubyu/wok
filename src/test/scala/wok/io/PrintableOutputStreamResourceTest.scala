package scalax.io

import org.specs2.mutable._
import wok.Helpers._
import wok.csv.Writer



class PrintableOutputStreamResourceTest extends SpecificationWithJUnit {

  "PrintableOutputStreamResource.println" should {
    "println data to the last of a OutputStream" in {
      val out = new TestOutputStream()
      val res = new PrintableOutputStreamResource(out)
      out.write("a\n".getBytes())
      out.toString() mustEqual "a\n"
      res.println("b")(Writer())
      res.println("c")(Writer())
      out.toString() mustEqual "a\nb\nc\n"
    }
  }

  "PrintableOutputStreamResource.printf" should {
    "print data to the last of a OutputStream" in {
      val out = new TestOutputStream()
      val res = new PrintableOutputStreamResource(out)
      out.write("a".getBytes())
      out.toString() mustEqual "a"
      res.printf("b")(Writer())
      res.printf("c")(Writer())
      out.toString() mustEqual "ab c "
    }
  }

  "PrintableOutputStreamResource.print" should {
    "print data to the last of a OutputStream" in {
      val out = new TestOutputStream()
      val res = new PrintableOutputStreamResource(out)
      out.write("a".getBytes())
      out.toString() mustEqual "a"
      res.print("b")(Writer())
      res.print("c")(Writer())
      out.toString() mustEqual "abc"
    }
  }
}