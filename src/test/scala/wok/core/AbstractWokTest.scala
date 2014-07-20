
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
      implicit val reader = new Reader()
    }

    "support accesses for Reader's parameters" in {
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

    "support accesses for Writer's parameters" in {
      "OFS" in {
        val wok = new Wok()
        wok.OFS mustEqual " "
        wok.OFS("a").OFS mustEqual "a"
      }

      "ORS" in {
        val wok = new Wok()
        wok.ORS mustEqual "\n"
        wok.ORS("a").ORS mustEqual "a"
      }

      "OFQ" in {
        val wok = new Wok()
        wok.OFQ mustEqual QuoteOption()
        wok.OFQ(QuoteOption().All()).OFQ mustEqual QuoteOption().All()
      }

      "OCD" in {
        val wok = new Wok()
        wok.OCD mustEqual Codec.default
        wok.OCD(Codec.ISO8859).OCD mustEqual Codec.ISO8859
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

    "privide a function InputStream.open" in {
      import Helpers.OpenableInputStream

      "open an InputStream" in {
        val in = new ByteArrayInputStream("a b c".getBytes())
        val wok = new Wok {
          def open = in.open()
        }
        wok.open.next.field mustEqual List("a", "b", "c")
      }

      "open an InputStream with Reader" in {
        val in = new ByteArrayInputStream("a-b-c".getBytes())
        val wok = new Wok {
          def open = in.open()(new Reader().FS("-"))
        }
        wok.open.next.field mustEqual List("a", "b", "c")
      }
    }

    "privide a function Path.open" in {
      import Helpers.OpenablePath

      trait scope extends Scope {
        val p = Path.createTempFile()
      }

      "open a existent file" in new scope {
        p.write("a b c")
        val wok = new Wok {
          def open = p.open()
        }
        wok.open.next().field mustEqual List("a", "b", "c")
      }

      "open a non-existent file" in new scope {
        val wok = new Wok {
          def open = Path("non-existent").open()
        }
        wok.open must throwA[FileNotFoundException]
      }

      "open a existent file with Reader" in new scope {
        p.write("a-b-c")
        val wok = new Wok {
          def open = p.open()(new Reader().FS("-"))
        }
        wok.open.next().field mustEqual List("a", "b", "c")
      }
    }

    "provide functions OutputStream.print/println" in {
      import Helpers.PrintableOutputStream

      trait scope extends Scope {
        val out = new ByteArrayOutputStream()
        def result = new String(out.toByteArray)
      }

      "println()" in new scope {
        new Wok {
          out.println()
          out.println()
        }
        result mustEqual "\n\n"
      }

      "println(Any)" in new scope {
        new Wok {
          out.println("a")
          out.println("b")
        }
        result mustEqual "a\nb\n"
      }

      "print(Any)" in new scope {
        new Wok {
          out.print("a")
          out.print("b")
        }
        result mustEqual "ab"
      }

      "println(Seq, Any)" in new scope {
        new Wok {
          out.println(Seq("a"))
          out.println("b")
        }
        result mustEqual "a\nb\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          out.print(Seq("a"))
          out.print("b")
        }
        result mustEqual "ab"
      }

      "println() with Writer" in new scope {
        new Wok {
          out.println()(new Writer().OFQ(QuoteOption().All()))
          out.println()(new Writer().OFQ(QuoteOption().All()))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(new Writer().OFQ(QuoteOption().All()))
          out.println("b")(new Writer().OFQ(QuoteOption().All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(new Writer().OFQ(QuoteOption().All()))
          out.print("b")(new Writer().OFQ(QuoteOption().All()))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"))(new Writer().OFQ(QuoteOption().All()))
          out.println("b")(new Writer().OFQ(QuoteOption().All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"))(new Writer().OFQ(QuoteOption().All()))
          out.print("b")(new Writer().OFQ(QuoteOption().All()))
        }
        result mustEqual "\"a\"\"b\""
      }
    }

    "provide functions Path.print/println" in {
      import Helpers.PrintablePath

      trait scope extends Scope {
        val p = Path.createTempFile()
      }

      "println to non-existing file" in {
        val p = Path("testoutput")
        new Wok {
          p.println("a")
          p.println("b")
        }
        val result = p.string
        p.delete()
        result mustEqual "a\nb\n"
      }

      "println()" in new scope {
        new Wok {
          p.println()
          p.println()
        }
        p.string mustEqual "\n\n"
      }

      "println(Any)" in new scope {
        new Wok {
          p.println("a")
          p.println("b")
        }
        p.string mustEqual "a\nb\n"
      }

      "print(Any)" in new scope {
        new Wok {
          p.print("a")
          p.print("b")
        }
        p.string mustEqual "ab"
      }

      "println(Seq, Any)" in new scope {
        new Wok {
          p.println(Seq("a"))
          p.println("b")
        }
        p.string mustEqual "a\nb\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          p.print(Seq("a"))
          p.print("b")
        }
        p.string mustEqual "ab"
      }

      "println() with Writer" in new scope {
        new Wok {
          p.println()(new Writer().OFQ(QuoteOption().All()))
          p.println()(new Writer().OFQ(QuoteOption().All()))
        }
        p.string mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          p.println("a")(new Writer().OFQ(QuoteOption().All()))
          p.println("b")(new Writer().OFQ(QuoteOption().All()))
        }
        p.string mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          p.print("a")(new Writer().OFQ(QuoteOption().All()))
          p.print("b")(new Writer().OFQ(QuoteOption().All()))
        }
        p.string mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          p.println(Seq("a"))(new Writer().OFQ(QuoteOption().All()))
          p.println("b")(new Writer().OFQ(QuoteOption().All()))
        }
        p.string mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          p.print(Seq("a"))(new Writer().OFQ(QuoteOption().All()))
          p.print("b")(new Writer().OFQ(QuoteOption().All()))
        }
        p.string mustEqual "\"a\"\"b\""
      }
    }

    "provide functions Wok.print/println" in {

      trait scope extends Scope {
        val outStream = new ByteArrayOutputStream
        val out = new PrintStream(new BufferedOutputStream(outStream), true, "utf-8")
        def result = new String(outStream.toByteArray, "utf-8")
      }

      "print(Any)" in new scope {
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

      "println(Any)" in new scope {
        Console.withOut(out) {
          new Wok {
            println("a")
            println("b")
          }
        }
        result mustEqual "a\nb\n"
      }

      "print(Seq, Any)" in new scope {
        Console.withOut(out) {
          new Wok {
            print(Seq("a"))
            print("b")
          }
        }
        result mustEqual "ab"
      }

      "println(Seq, Any)" in new scope {
        Console.withOut(out) {
          new Wok {
            println(Seq("a"))
            println("b")
          }
        }
        result mustEqual "a\nb\n"
      }

      "print(Any) with Writer" in new scope {
        Console.withOut(out) {
          new Wok {
            print("a")(new Writer().OFQ(QuoteOption().All()))
            print("b")(new Writer().OFQ(QuoteOption().All()))
          }
        }
        result mustEqual "\"a\"\"b\""
      }

      "println() with Writer" in new scope {
        Console.withOut(out) {
          new Wok {
            println()(new Writer().OFQ(QuoteOption().All()))
            println()(new Writer().OFQ(QuoteOption().All()))
          }
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        Console.withOut(out) {
          new Wok {
            println("a")(new Writer().OFQ(QuoteOption().All()))
            println("b")(new Writer().OFQ(QuoteOption().All()))
          }
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        Console.withOut(out) {
          new Wok {
            print(Seq("a"))(new Writer().OFQ(QuoteOption().All()))
            print("b")(new Writer().OFQ(QuoteOption().All()))
          }
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        Console.withOut(out) {
          new Wok {
            println(Seq("a"))(new Writer().OFQ(QuoteOption().All()))
            println("b")(new Writer().OFQ(QuoteOption().All()))
          }
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }
    }
  }
}
