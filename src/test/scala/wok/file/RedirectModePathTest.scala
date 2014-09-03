package scalax.file.defaultfs

import org.specs2.mutable._
import wok.reflect.AbstractWok
import scalax.file.Path


class RedirectModePathTest extends SpecificationWithJUnit {

  val wok = new AbstractWok {
    def runScript(){}
  }

  "RedirectModePath.println" should {
    "truncate contents already exists and println data" in {
      val p = Path.createTempFile()
      p.write("a\n")
      val rp = new RedirectModePath(p)
      rp.string mustEqual "a\n"
      rp.println("b")(wok)
      rp.println("c")(wok)
      rp.string mustEqual "b\nc\n"
    }
  }

  "RedirectModePath.printf" should {
    "truncate contents already exists and print data" in {
      val p = Path.createTempFile()
      p.write("a")
      val rp = new RedirectModePath(p)
      rp.string mustEqual "a"
      rp.printf("b")(wok)
      rp.printf("c")(wok)
      rp.string mustEqual "b c "
    }
  }

  "RedirectModePath.print" should {
    "truncate contents already exists and print data" in {
      val p = Path.createTempFile()
      p.write("a")
      val rp = new RedirectModePath(p)
      rp.string mustEqual "a"
      rp.print("b")(wok)
      rp.print("c")(wok)
      rp.string mustEqual "bc"
    }
  }
}
