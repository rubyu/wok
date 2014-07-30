package wok.core

import java.io.ByteArrayOutputStream

import org.specs2.mutable._


class MainTest extends SpecificationWithJUnit {

  "Main.main" should {
    "execute a script of Wok" in {
      val out = new ByteArrayOutputStream()
      Stdio.withOut(out) {
        Main.main(Array("-b" ,"print(\"a\")"))
      }
      out.toString mustEqual "a"
    }
  }
}