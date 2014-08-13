package scalax.file.defaultfs

import org.specs2.mutable._
import wok.csv.Writer
import scalax.file.Path


class AppendModePathTest extends SpecificationWithJUnit {

  "AppendModePath.println" should {
    "println data to the last of a file" in {
      val p = Path.createTempFile()
      p.write("a\n")
      val rp = new AppendModePath(p)
      rp.string mustEqual "a\n"
      rp.println("b")(Writer())
      rp.println("c")(Writer())
      rp.string mustEqual "a\nb\nc\n"
    }
  }

  "AppendModePath.print" should {
    "print data to the last of a file" in {
      val p = Path.createTempFile()
      p.write("a")
      val rp = new AppendModePath(p)
      rp.string mustEqual "a"
      rp.print("b")(Writer())
      rp.print("c")(Writer())
      rp.string mustEqual "abc"
    }
  }
}
