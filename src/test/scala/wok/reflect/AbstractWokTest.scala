
package wok.reflect

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.csv.{Quote, Writer, Reader}
import wok.core.Stdio
import java.io._
import scalax.file.Path
import scalax.io.{StandardOpenOption, Resource, Codec}


class AbstractWokTest extends SpecificationWithJUnit {
  "AbstractWok" should {

    class Wok extends AbstractWok {
      val args = List()
      def runScript(){}
    }

    "support accesses for Reader's parameters" in {
      "FS" in {
        val wok = new Wok()
        wok.FS.toString mustEqual "[ \\t]+"
        wok.FS("a".r).FS.toString mustEqual "a"
        wok.FS("a").FS.toString mustEqual "a"
        wok.FS("").FS.toString mustEqual "(?!.)."
      }

      "RS" in {
        val wok = new Wok()
        wok.RS.toString mustEqual "\\r\\n|\\r|\\n"
        wok.RS("a".r).RS.toString mustEqual "a"
        wok.RS("a").RS.toString mustEqual "a"
        wok.RS("").RS.toString mustEqual "(?!.)."
      }

      "FQ" in {
        val wok = new Wok()
        wok.FQ mustEqual Quote.None()
        wok.FQ(Quote All()).FQ mustEqual Quote.All()
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
        wok.OFQ mustEqual Quote.None()
        wok.OFQ(Quote All()).OFQ mustEqual Quote.All()
      }

      "OCD" in {
        val wok = new Wok()
        wok.OCD mustEqual Codec.default
        wok.OCD(Codec.ISO8859).OCD mustEqual Codec.ISO8859
      }
    }

    "privide a function InputStream.csv" in {
      import Helpers.ExtendedInputStream

      "open an InputStream" in {
        val in = new ByteArrayInputStream("a b c".getBytes)
        val wok = new Wok {
          def open = in.csv
        }
        wok.open.next mustEqual List("a", "b", "c")
      }

      "open an InputStream with Reader" in {
        val in = new ByteArrayInputStream("a-b-c".getBytes)
        val wok = new Wok {
          def open = in.csv(Reader.FS("-"))
        }
        wok.open.next mustEqual List("a", "b", "c")
      }
    }

    "add extended functions print/println to OutputStream" in {
      import Helpers.ExtendedOutputStream

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
          out.println(Seq("a"), "b")
        }
        result mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          out.print(Seq("a"), "b")
        }
        result mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          out.println()(Writer OFQ(Quote All()))
          out.println()(Writer OFQ(Quote All()))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(Writer OFQ(Quote All()))
          out.println("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(Writer OFQ(Quote All()))
          out.print("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\""
      }
    }

    "add a extended function <| to Path" in {
      import Helpers.ExtendedPath

      trait scope extends Scope {
        val out = Path.createTempFile().<!
        def result = out.string
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
          out.println(Seq("a"), "b")
        }
        result mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          out.print(Seq("a"), "b")
        }
        result mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          out.println()(Writer OFQ(Quote All()))
          out.println()(Writer OFQ(Quote All()))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(Writer OFQ(Quote All()))
          out.println("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(Writer OFQ(Quote All()))
          out.print("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\""
      }
    }

    "add a extended function <<| to Path" in {
      import Helpers.ExtendedPath

      trait scope extends Scope {
        val out = Path.createTempFile().<<!
        def result = out.string
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
          out.println(Seq("a"), "b")
        }
        result mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          out.print(Seq("a"), "b")
        }
        result mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          out.println()(Writer OFQ(Quote All()))
          out.println()(Writer OFQ(Quote All()))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(Writer OFQ(Quote All()))
          out.println("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(Writer OFQ(Quote All()))
          out.print("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\""
      }
    }

    "add a extended function <| to String" in {
      import Helpers.ExtendedPathString

      trait scope extends Scope {
        val out = Path.createTempFile().path.<!
        def result = out.string
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
          out.println(Seq("a"), "b")
        }
        result mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          out.print(Seq("a"), "b")
        }
        result mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          out.println()(Writer OFQ(Quote All()))
          out.println()(Writer OFQ(Quote All()))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(Writer OFQ(Quote All()))
          out.println("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(Writer OFQ(Quote All()))
          out.print("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\""
      }
    }

    "add a extended function <<| to String" in {
      import Helpers.ExtendedPathString

      trait scope extends Scope {
        val out = Path.createTempFile().path.<<!
        def result = out.string
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
          out.println(Seq("a"), "b")
        }
        result mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          out.print(Seq("a"), "b")
        }
        result mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          out.println()(Writer OFQ(Quote All()))
          out.println()(Writer OFQ(Quote All()))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(Writer OFQ(Quote All()))
          out.println("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(Writer OFQ(Quote All()))
          out.print("b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"), "b")(Writer OFQ(Quote All()))
        }
        result mustEqual "\"a\" \"b\""
      }
    }

    "provide functions print/println" in {

      trait scope extends Scope {
        val out = new ByteArrayOutputStream
        def result = new String(out.toByteArray, "utf-8")
      }

      "print(Any)" in new scope {
        Stdio.withOut(out) {
          new Wok {
            print("a")
            print("b")
          }
        }
        result mustEqual "ab"
      }

      "println()" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println()
            println()
          }
        }
        result mustEqual "\n\n"
      }

      "println(Any)" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println("a")
            println("b")
          }
        }
        result mustEqual "a\nb\n"
      }

      "print(Seq, Any)" in new scope {
        Stdio.withOut(out) {
          new Wok {
            print(Seq("a"), "b")
          }
        }
        result mustEqual "a b"
      }

      "println(Seq, Any)" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println(Seq("a"), "b")
          }
        }
        result mustEqual "a b\n"
      }

      "print(Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            print("a")(Writer OFQ(Quote All()))
            print("b")(Writer OFQ(Quote All()))
          }
        }
        result mustEqual "\"a\"\"b\""
      }

      "println() with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println()(Writer OFQ(Quote All()))
            println()(Writer OFQ(Quote All()))
          }
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println("a")(Writer OFQ(Quote All()))
            println("b")(Writer OFQ(Quote All()))
          }
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            print(Seq("a"), "b")(Writer OFQ(Quote All()))
          }
        }
        result mustEqual "\"a\" \"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println(Seq("a"), "b")(Writer OFQ(Quote All()))
          }
        }
        result mustEqual "\"a\" \"b\"\n"
      }
    }

    "ExtendedInputStreamResource" in {
      import Helpers.ExtendedInputStreamResource

      "add a extended function |> to InputStreamResource" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          val wok = new Wok {
            def string = out.inputStream #> { _.read().toChar.toString }
          }
          wok.string mustEqual "a"
        }
      }
    }

   "ExtendedOutputStreamResource" in {
     import Helpers.ExtendedOutputStreamResource

      "add a extended function <<| to OutputStreamResource" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out.outputStream(StandardOpenOption.Append) #<< { _.write("b".getBytes) }
          }
          out.string mustEqual "ab"
        }
      }
    }

    "ExtendedPath" in {
      import Helpers.ExtendedPath

      "add a extended function |> to Path" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          val wok = new Wok {
            def string = out #> { _.read().toChar.toString }
          }
          wok.string mustEqual "a"
        }
      }

      "add a extended function <| to Path" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out #< { _.write("b".getBytes) }
          }
          out.string mustEqual "b"
        }
      }

      "add a extended function <<| to Path" in {
        "truncate contents already exists and write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out #<< { _.write("b".getBytes) }
          }
          out.string mustEqual "ab"
        }
      }
    }

    "ExtendedPathString" in {
      import Helpers.ExtendedPathString

      "add a extended function |> to String" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          val wok = new Wok {
            def string = out.path #> { _.read().toChar.toString }
          }
          wok.string mustEqual "a"
        }
      }

      "add a extended function <| to String" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out.path #< { _.write("b".getBytes) }
          }
          out.string mustEqual "b"
        }
      }

      "add a extended fucntion <<| to String" in {
        "truncate contents already exists and write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out.path #<< { _.write("b".getBytes) }
          }
          out.string mustEqual "ab"
        }
      }
    }
  }
}
