
package wok.reflect

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.csv.{Quote, Writer, Reader}
import wok.core.Stdio
import java.io._
import scalax.file.Path
import scalax.io.{Resource, Codec}


class AbstractWokTest extends SpecificationWithJUnit {
  "AbstractWok" should {

    class Wok extends AbstractWok {
      val arg = List()
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
        wok.FQ(Quote.All).FQ mustEqual Quote.All
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
        wok.OFQ(Quote.All).OFQ mustEqual Quote.All
      }

      "OCD" in {
        val wok = new Wok()
        wok.OCD mustEqual Codec.default
        wok.OCD(Codec.ISO8859).OCD mustEqual Codec.ISO8859
      }
    }

    "privide a function InputStream.csv" in {
      import Helpers.OpenableInputStream

      "open an InputStream" in {
        val in = new ByteArrayInputStream("a b c".getBytes())
        val wok = new Wok {
          def open = in.csv
        }
        wok.open.next mustEqual List("a", "b", "c")
      }

      "open an InputStream with Reader" in {
        val in = new ByteArrayInputStream("a-b-c".getBytes())
        val wok = new Wok {
          def open = in.csv(Reader.FS("-"))
        }
        wok.open.next mustEqual List("a", "b", "c")
      }
    }

    "privide a function String.csv" in {
      import Helpers.OpenableString

      "open an InputStream" in {
        val p = Path.createTempFile()
        p.write("a b c")
        val wok = new Wok {
          def open = p.path.csv
        }
        wok.open.next mustEqual List("a", "b", "c")
      }

      "throw FileNotFoundException when open non-existent file" in {
        val p = Path("non-existent")
        val wok = new Wok {
          def open = p.path.csv
        }
        wok.open must throwA[FileNotFoundException]
      }

      "open an InputStream with Reader" in {
        val p = Path.createTempFile()
        p.write("a-b-c")
        val wok = new Wok {
          def open = p.path.csv(Reader.FS("-"))
        }
        wok.open.next mustEqual List("a", "b", "c")
      }
    }

    "privide a function InputStreamResource.csv" in {
      import Helpers.OpenableInputStreamResource

      "open an InputStreamResource" in {
        val in = Resource.fromInputStream(new ByteArrayInputStream("a b c".getBytes()))
        val wok = new Wok {
          def open = in.csv
        }
        wok.open.next mustEqual List("a", "b", "c")
      }

      "open an InputStreamResource with Reader" in {
        val in = Resource.fromInputStream(new ByteArrayInputStream("a-b-c".getBytes()))
        val wok = new Wok {
          def open = in.csv(Reader.FS("-"))
        }
        wok.open.next mustEqual List("a", "b", "c")
      }
    }

    "privide a function Path.csv" in {
      import Helpers.OpenablePath

      trait scope extends Scope {
        val p = Path.createTempFile()
      }

      "open a existent file" in new scope {
        p.write("a b c")
        val wok = new Wok {
          def open = p.csv
        }
        wok.open.next mustEqual List("a", "b", "c")
      }

      "throw FileNotFoundException when open a non-existent file" in new scope {
        val wok = new Wok {
          def open = Path("non-existent").csv
        }
        wok.open must throwA[FileNotFoundException]
      }

      "open a existent file with Reader" in new scope {
        p.write("a-b-c")
        val wok = new Wok {
          def open = p.csv(Reader.FS("-"))
        }
        wok.open.next mustEqual List("a", "b", "c")
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
          out.println()(Writer.OFQ(Quote.All))
          out.println()(Writer.OFQ(Quote.All))
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          out.println("a")(Writer.OFQ(Quote.All))
          out.println("b")(Writer.OFQ(Quote.All))
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          out.print("a")(Writer.OFQ(Quote.All))
          out.print("b")(Writer.OFQ(Quote.All))
        }
        result mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          out.println(Seq("a"), "b")(Writer.OFQ(Quote.All))
        }
        result mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          out.print(Seq("a"), "b")(Writer.OFQ(Quote.All))
        }
        result mustEqual "\"a\" \"b\""
      }
    }

    "provide functions Path.print/println" in {
      import Helpers.PrintablePath

      trait scope extends Scope {
        val p = Path.createTempFile()
      }

      "println to non-existing file" in new scope {
        p.delete()
        new Wok {
          p.println("a")
          p.println("b")
        }
        p.string mustEqual "a\nb\n"
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
          p.println(Seq("a"), "b")
        }
        p.string mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          p.print(Seq("a"), "b")
        }
        p.string mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          p.println()(Writer.OFQ(Quote.All))
          p.println()(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          p.println("a")(Writer.OFQ(Quote.All))
          p.println("b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          p.print("a")(Writer.OFQ(Quote.All))
          p.print("b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          p.println(Seq("a"), "b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          p.print(Seq("a"), "b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\" \"b\""
      }
    }

    "provide functions String.print/println" in {
      import Helpers.PrintableString

      trait scope extends Scope {
        val p = Path.createTempFile()
      }

      "println to non-existing file" in new scope {
        p.delete()
        new Wok {
          p.path.println("a")
          p.path.println("b")
        }
        val result = p.string
        p.delete()
        result mustEqual "a\nb\n"
      }

      "println()" in new scope {
        new Wok {
          p.path.println()
          p.path.println()
        }
        p.string mustEqual "\n\n"
      }

      "println(Any)" in new scope {
        new Wok {
          p.path.println("a")
          p.path.println("b")
        }
        p.string mustEqual "a\nb\n"
      }

      "print(Any)" in new scope {
        new Wok {
          p.path.print("a")
          p.path.print("b")
        }
        p.string mustEqual "ab"
      }

      "println(Seq, Any)" in new scope {
        new Wok {
          p.path.println(Seq("a"), "b")
        }
        p.string mustEqual "a b\n"
      }

      "print(Seq, Any)" in new scope {
        new Wok {
          p.path.print(Seq("a"), "b")
        }
        p.string mustEqual "a b"
      }

      "println() with Writer" in new scope {
        new Wok {
          p.path.println()(Writer.OFQ(Quote.All))
          p.path.println()(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        new Wok {
          p.path.println("a")(Writer.OFQ(Quote.All))
          p.path.println("b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Any) with Writer" in new scope {
        new Wok {
          p.path.print("a")(Writer.OFQ(Quote.All))
          p.path.print("b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\"\"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        new Wok {
          p.path.println(Seq("a"), "b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\" \"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        new Wok {
          p.path.print(Seq("a"), "b")(Writer.OFQ(Quote.All))
        }
        p.string mustEqual "\"a\" \"b\""
      }
    }

    "provide functions Wok.print/println" in {

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
            print("a")(Writer.OFQ(Quote.All))
            print("b")(Writer.OFQ(Quote.All))
          }
        }
        result mustEqual "\"a\"\"b\""
      }

      "println() with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println()(Writer.OFQ(Quote.All))
            println()(Writer.OFQ(Quote.All))
          }
        }
        result mustEqual "\n\n"
      }

      "println(Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println("a")(Writer.OFQ(Quote.All))
            println("b")(Writer.OFQ(Quote.All))
          }
        }
        result mustEqual "\"a\"\n\"b\"\n"
      }

      "print(Seq, Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            print(Seq("a"), "b")(Writer.OFQ(Quote.All))
          }
        }
        result mustEqual "\"a\" \"b\""
      }

      "println(Seq, Any) with Writer" in new scope {
        Stdio.withOut(out) {
          new Wok {
            println(Seq("a"), "b")(Writer.OFQ(Quote.All))
          }
        }
        result mustEqual "\"a\" \"b\"\n"
      }
    }
  }
}
