package scalax.io

import org.specs2.mutable._
import wok.reflect.AbstractWok
import wok.Helpers._


class PrintableOutputStreamResourceTest extends SpecificationWithJUnit {

  val wok = new AbstractWok {
    def runScript(){}
  }

  "PrintableOutputStreamResource.println" should {
    "println data to the last of a OutputStream" in {
      val out = new TestOutputStream()
      val res = new PrintableOutputStreamResource(out)
      out.write("a\n".getBytes())
      out.toString() mustEqual "a\n"
      res.println("b")(wok)
      res.println("c")(wok)
      out.toString() mustEqual "a\nb\nc\n"
    }
  }

  "PrintableOutputStreamResource.printf" should {
    "print data to the last of a OutputStream" in {
      val out = new TestOutputStream()
      val res = new PrintableOutputStreamResource(out)
      out.write("a".getBytes())
      out.toString() mustEqual "a"
      res.printf("b")(wok)
      res.printf("c")(wok)
      out.toString() mustEqual "ab c "
    }
  }

  "PrintableOutputStreamResource.print" should {
    "print data to the last of a OutputStream" in {
      val out = new TestOutputStream()
      val res = new PrintableOutputStreamResource(out)
      out.write("a".getBytes())
      out.toString() mustEqual "a"
      res.print("b")(wok)
      res.print("c")(wok)
      out.toString() mustEqual "abc"
    }
  }
}
