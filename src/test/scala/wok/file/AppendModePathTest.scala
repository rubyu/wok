package scalax.file.defaultfs

import org.specs2.mutable._
import wok.reflect.AbstractWok
import scalax.file.Path


class AppendModePathTest extends SpecificationWithJUnit {

  val wok = new AbstractWok {
    def runScript(){}
  }

  "AppendModePath.println" should {

    "println data to the last of a file" in {
      val p = Path.createTempFile()
      p.write("a\n")
      val rp = new AppendModePath(p)
      rp.string mustEqual "a\n"
      rp.println("b")(wok)
      rp.println("c")(wok)
      rp.string mustEqual "a\nb\nc\n"
    }
  }

  "AppendModePath.printf" should {
    "print data to the last of a file" in {
      val p = Path.createTempFile()
      p.write("a")
      val rp = new AppendModePath(p)
      rp.string mustEqual "a"
      rp.printf("b")(wok)
      rp.printf("c")(wok)
      rp.string mustEqual "ab c "
    }
  }

  "AppendModePath.print" should {
    "print data to the last of a file" in {
      val p = Path.createTempFile()
      p.write("a")
      val rp = new AppendModePath(p)
      rp.string mustEqual "a"
      rp.print("b")(wok)
      rp.print("c")(wok)
      rp.string mustEqual "abc"
    }
  }
}
