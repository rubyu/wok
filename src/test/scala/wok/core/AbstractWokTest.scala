
package wok.core

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.csv.{QuoteOption, Writer, Reader}
import java.io._
import scalax.file.Path
import scala.Console
import scalax.io.Codec


class AbstractWokTest extends SpecificationWithJUnit {
  "AbstractWok" should {

    class Wok extends AbstractWok {
      val arg = List()
      implicit val writer = new Writer()
      val reader = new Reader()
    }

    "support accesses for Parser's parameters" in {
      "FS" in {
        val wok = new Wok()
        wok.FS.toString mustEqual "\\s+"
        wok.FS("a".r).FS.toString mustEqual "a"
        wok.FS("a").FS.toString mustEqual "a"
        wok.FS("").FS.toString mustEqual "(?!.)."
      }

      "RS" in {
        val wok = new Wok()
        wok.RS.toString mustEqual "(\\r\\n|\\r|\\n)"
        wok.RS("a".r).RS.toString mustEqual "a"
        wok.RS("a").RS.toString mustEqual "a"
        wok.RS("").RS.toString mustEqual "(?!.)."
      }

      "FQ" in {
        val wok = new Wok()
        wok.FQ mustEqual QuoteOption()
        wok.FQ(QuoteOption().All()).FQ mustEqual QuoteOption().All()
      }

      "CD" in {
        val wok = new Wok()
        wok.CD mustEqual Codec.default
        wok.CD(Codec.ISO8859).CD mustEqual Codec.ISO8859
      }
    }

    "support Iterator[_] and Unit in the type of the result" in {
      import Helpers.{CompletableIterator, CompletableAny}

      "Unit" in {
        new Wok { reader.open(new StringReader("a")) foreach { row => } complete() }
        success
      }

      "Iterator[Row]" in {
        new Wok { reader.open(new StringReader("a")) complete() }
        success
      }

      "Iterator[Unit]" in {
        new Wok { reader.open(new StringReader("a")) map {row => ()} complete() }
        success
      }
    }

    "provide variables for wok script's context" in {
      import Helpers.{CompletableIterator, CompletableAny}

      trait scope extends Scope {
        val outStream = new ByteArrayOutputStream
        val out = new PrintStream(new BufferedOutputStream(outStream), true, "utf-8")
        def result = new String(outStream.toByteArray, "utf-8")
      }

      "NR" in new scope {
        Console.withOut(out) {
          new Wok {
            {
              var NR: Long = -1
              val result = reader.open(new StringReader("a b c")) map { row => NR = row.id; row } map { row => print(NR) }
              result.complete()
            }
          }
        }
        result mustEqual "0"
      }

      "NF" in new scope {
        Console.withOut(out) {
          new Wok {
            {
              var NF: Int = -1
              val result = reader.open(new StringReader("a b c")) map { row => NF = row.field.size; row } map { row => print(NF) }
              result.complete()
            }
          }
        }
        result mustEqual "3"
      }

      "RT" in new scope {
        Console.withOut(out) {
          new Wok {
            {
              var RT: String = ""
              val result = reader.open(new StringReader("a b c")) map { row => RT = row.term; row } map { row => print(RT) }
              result.complete()
            }
          }
        }
        result mustEqual ""
      }

      "FT" in new scope {
        Console.withOut(out) {
          new Wok {
            {
              var FT: List[String] = List()
              val result = reader.open(new StringReader("a b c")) map { row => FT = row.sep; row } map { row => print(FT.size) }
              result.complete()
            }
          }
        }
        result mustEqual "2"
      }
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
