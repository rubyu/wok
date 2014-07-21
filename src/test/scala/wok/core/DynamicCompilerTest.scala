package wok.reflect

import org.specs2.mutable._
import java.io.{ByteArrayOutputStream, StringReader}


class DynamicCompilerTest extends SpecificationWithJUnit {
  "DynamicCompiler" should {
    "compile" in {
      val out = new ByteArrayOutputStream()
      Console.withOut(out) {
        Console.withIn(new StringReader("a b c")) {
          DynamicCompiler
            .compile(Nil, " map { row => println(row) } ", Nil)
            .create(Nil)
        }
      }
      new String(out.toByteArray) mustEqual "a b c\n"
    }
  }
}
