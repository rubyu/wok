package scalax.file.defaultfs

import org.specs2.mutable._
import wok.csv.Writer
import scalax.file.Path


class RedirectModePathTest extends SpecificationWithJUnit {
  "RedirectModePath.println" should {
    "truncate contents already exists and println data" in {
      val p = Path.createTempFile()
      p.write("a\n")
      val rp = new RedirectModePath(p)
      rp.string mustEqual "a\n"
      rp.println("b")(Writer())
      rp.println("c")(Writer())
      rp.string mustEqual "b\nc\n"
    }
  }

  "RedirectModePath.print" should {
    "truncate contents already exists and print data" in {
      val p = Path.createTempFile()
      p.write("a")
      val rp = new RedirectModePath(p)
      rp.string mustEqual "a"
      rp.print("b")(Writer())
      rp.print("c")(Writer())
      rp.string mustEqual "bc"
    }
  }
}
