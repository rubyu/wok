package wok.core

import java.io.{ByteArrayInputStream, Closeable, ByteArrayOutputStream}

import org.specs2.mutable._
import org.specs2.specification.Scope


class StdioTest extends SpecificationWithJUnit {

  trait scope extends Scope {
    trait CloseChecking extends Closeable {
      var closed = false
      override def close = closed = true
    }
    val out = new ByteArrayOutputStream with CloseChecking
    val in = new ByteArrayInputStream("a".getBytes()) with CloseChecking
  }

  "Stdio" should {
    "withOut not to close a given OutputStream" in new scope {
      Stdio.withOut(out) {
        Stdio.out.write("a")
        Stdio.out.write("b")
      }
      out.toString mustEqual "ab"
      out.closed must beFalse
    }

    "withErr not to close a given OutputStream" in new scope {
      Stdio.withErr(out) {
        Stdio.err.write("a")
        Stdio.err.write("b")
      }
      out.toString mustEqual "ab"
      out.closed must beFalse
    }

    "withIn not to close a given InputStream" in new scope {
      Stdio.withIn(in) {
        Stdio.in.string mustEqual "a"
      }
      in.closed must beFalse
    }
  }
}