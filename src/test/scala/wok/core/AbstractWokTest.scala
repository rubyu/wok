
package wok.core

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.csv.{Writer, Reader}
import java.io.{BufferedOutputStream, PrintStream, ByteArrayOutputStream}
import scalax.file.Path


class AbstractWokTest extends SpecificationWithJUnit {
  "AbstractWok" should {

    class Wok extends AbstractWok {
      def arg = List()
      implicit def writer = new Writer()
      def reader = new Reader()
    }

    "provide functions Path.print/Path.println" in {
      import Helpers.PrintablePath

      trait scope extends Scope {
        val p = Path.createTempFile()
      }

      "Path.println to non-existing file" in {
        val p = Path("testoutput")
        new Wok {
          p.println("a")
          p.println("b")
        }
        val result = p.string
        p.delete()
        result mustEqual "a\nb\n"
      }

      "Path.println()" in new scope {
        new Wok {
          p.println()
          p.println()
        }
        p.string mustEqual "\n\n"
      }

      "Path.println(x)" in new scope {
        new Wok {
          p.println("a")
          p.println("b")
        }
        p.string mustEqual "a\nb\n"
      }

      "Path.print(x)" in new scope {
        new Wok {
          p.print("a")
          p.print("b")
        }
        p.string mustEqual "ab"
      }
    }

    "provide functions print/println" in {

      trait scope extends Scope {
        val outStream = new ByteArrayOutputStream
        val out = new PrintStream(new BufferedOutputStream(outStream), true, "utf-8")
        def result = new String(outStream.toByteArray, "utf-8")
      }

      "print(x)" in new scope {
        Console.withOut(out) {
          new Wok {
            print("a")
            print("b")
          }
        }
        result mustEqual "ab"
      }

      "println()" in new scope {
        Console.withOut(out) {
          new Wok {
            println()
            println()
          }
        }
        result mustEqual "\n\n"
      }

      "println(x)" in new scope {
        Console.withOut(out) {
          new Wok {
            println("a")
            println("b")
          }
        }
        result mustEqual "a\nb\n"
      }
    }
  }
}
