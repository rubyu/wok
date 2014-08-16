package scalax.io

import org.specs2.mutable._
import wok.csv.Writer
import java.io.ByteArrayOutputStream


class AppendModeOutputStreamResourceTest extends SpecificationWithJUnit {

  "AppendModeOutputStreamResource.println" should {
    "println data to the last of a OutputStream" in {
      val out = new ByteArrayOutputStream()
      val res = new AppendModeOutputStreamResource(out)
      out.write("a\n".getBytes())
      out.toString() mustEqual "a\n"
      res.println("b")(Writer())
      res.println("c")(Writer())
      out.toString() mustEqual "a\nb\nc\n"
    }
  }

  "AppendModeOutputStreamResource.printf" should {
    "print data to the last of a OutputStream" in {
      val out = new ByteArrayOutputStream()
      val res = new AppendModeOutputStreamResource(out)
      out.write("a".getBytes())
      out.toString() mustEqual "a"
      res.printf("b")(Writer())
      res.printf("c")(Writer())
      out.toString() mustEqual "ab c "
    }
  }

  "AppendModeOutputStreamResource.print" should {
    "print data to the last of a OutputStream" in {
      val out = new ByteArrayOutputStream()
      val res = new AppendModeOutputStreamResource(out)
      out.write("a".getBytes())
      out.toString() mustEqual "a"
      res.print("b")(Writer())
      res.print("c")(Writer())
      out.toString() mustEqual "abc"
    }
  }
}
