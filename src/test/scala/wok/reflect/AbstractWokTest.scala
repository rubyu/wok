
package wok.reflect

import java.nio.charset.StandardCharsets

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.Helpers._
import wok.csv.Quote
import wok.core.Stdio
import scalax.file.Path
import scalax.io.{StandardOpenOption, Codec}


class AbstractWokTest extends SpecificationWithJUnit {
  "AbstractWok" should {

    class Wok extends AbstractWok {
      val args = List()
      def runScript(){}
    }

    "In" in {
      val p1 = Path.createTempFile()
      p1.write("a b c")
      val p2 = Path.createTempFile()
      p2.write("d e f")
      "protect iherited mutable parameters" in {
        val wok = new AbstractWok {
          val args = List()
          def runScript(){}
          def in = In(p1.path, p2.path) { row => FS("-") OFS("-") } .toList
        }
        wok.in
        wok.FS mustNotEqual "-"
        wok.OFS mustNotEqual "-"
      }
    }

    "In" in {
      val p1 = Path.createTempFile()
      p1.write("a b c")
      val p2 = Path.createTempFile()
      p2.write("d e f")
      val wok = new AbstractWok {
        val args = List()
        def runScript(){}
        def in = In(p1.path, p2.path) { row => (row, ARGV, ARGC, ARGIND, FILENAME, FNR, NR, NF, FT, RT) } .toList
      }
      val result = wok.in
      result.size mustEqual 2
      result(0)._1.size mustEqual 3
      result(0)._1.source mustEqual "a b c"
      result(0)._2 mustEqual List(p1.path, p2.path)
      result(0)._3 mustEqual 2
      result(0)._4 mustEqual 0
      result(0)._5 mustEqual p1.path
      result(0)._6 mustEqual 0
      result(0)._7 mustEqual 0
      result(0)._8 mustEqual 3
      result(0)._9 mustEqual List(" ", " ")
      result(0)._10 mustEqual ""

      result(1)._1.size mustEqual 3
      result(1)._1.source mustEqual "d e f"
      result(1)._2 mustEqual List(p1.path, p2.path)
      result(1)._3 mustEqual 2
      result(1)._4 mustEqual 1
      result(1)._5 mustEqual p2.path
      result(1)._6 mustEqual 0
      result(1)._7 mustEqual 1
      result(1)._8 mustEqual 3
      result(1)._9 mustEqual List(" ", " ")
      result(1)._10 mustEqual ""
    }

    "support accesses for Reader's parameters" in {
      "FS" in {
        val wok = new Wok()
        wok.FS.toString mustEqual "[ \\t]+"
        wok.FS("a".r).FS.toString mustEqual "a"
        wok.FS("a").FS.toString mustEqual "a"
        wok.FS("").FS.toString mustEqual "(?!.)."
        wok.FS('a').FS.toString mustEqual "a"
      }

      "RS" in {
        val wok = new Wok()
        wok.RS.toString mustEqual "\\r\\n|\\r|\\n"
        wok.RS("a".r).RS.toString mustEqual "a"
        wok.RS("a").RS.toString mustEqual "a"
        wok.RS("").RS.toString mustEqual "(?!.)."
        wok.RS('a').RS.toString mustEqual "a"
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
        wok.OFS('a').OFS mustEqual "a"
      }

      "ORS" in {
        val wok = new Wok()
        wok.ORS mustEqual "\n"
        wok.ORS("a").ORS mustEqual "a"
        wok.ORS('a').ORS mustEqual "a"
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

    "add extended functions print/println to OutputStream" in {
      import Helpers.ExtendedOutputStream

      trait scope extends Scope {
        val out = new TestOutputStream()

        def result = new String(out.toByteArray, StandardCharsets.UTF_8)
      }

      "println()" in new scope {
        new Wok {
          out.println()
          out.println()
        }
        result mustEqual "\n\n"
      }

      "printf()" in new scope {
        new Wok {
          out.printf()
          out.printf()
        }
        result mustEqual "  "
      }

      "print()" in new scope {
        new Wok {
          out.print()
          out.print()
        }
        result mustEqual ""
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
          out.printf("a")
          out.printf("b")
        }
        result mustEqual "a b "
      }

      "print(Any)" in new scope {
        new Wok {
          out.print("a")
          out.print("b")
        }
        result mustEqual "ab"
      }
    }

    "add a extended function !< to Path" in {
      import Helpers.ExtendedPath

      trait scope extends Scope {
        val out = Path.createTempFile().!<

        def result = out.string
      }

      "println()" in new scope {
        new Wok {
          out.println()
          out.println()
        }
        result mustEqual "\n\n"
      }

      "printf()" in new scope {
        new Wok {
          out.printf()
          out.printf()
        }
        result mustEqual "  "
      }

      "print()" in new scope {
        new Wok {
          out.print()
          out.print()
        }
        result mustEqual ""
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
          out.printf("a")
          out.printf("b")
        }
        result mustEqual "a b "
      }

      "print(Any)" in new scope {
        new Wok {
          out.print("a")
          out.print("b")
        }
        result mustEqual "ab"
      }
    }

    "add a extended function !<< to Path" in {
      import Helpers.ExtendedPath

      trait scope extends Scope {
        val out = Path.createTempFile().!<<
        def result = out.string
      }

      "println()" in new scope {
        new Wok {
          out.println()
          out.println()
        }
        result mustEqual "\n\n"
      }

      "printf()" in new scope {
        new Wok {
          out.printf()
          out.printf()
        }
        result mustEqual "  "
      }

      "print()" in new scope {
        new Wok {
          out.print()
          out.print()
        }
        result mustEqual ""
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
          out.printf("a")
          out.printf("b")
        }
        result mustEqual "a b "
      }

      "print(Any)" in new scope {
        new Wok {
          out.print("a")
          out.print("b")
        }
        result mustEqual "ab"
      }

    }

    "add a extended function !< to String" in {
      import Helpers.ExtendedPathString

      trait scope extends Scope {
        val out = Path.createTempFile().path.!<
        def result = out.string
      }

      "println()" in new scope {
        new Wok {
          out.println()
          out.println()
        }
        result mustEqual "\n\n"
      }

      "printf()" in new scope {
        new Wok {
          out.printf()
          out.printf()
        }
        result mustEqual "  "
      }

      "print()" in new scope {
        new Wok {
          out.print()
          out.print()
        }
        result mustEqual ""
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
          out.printf("a")
          out.printf("b")
        }
        result mustEqual "a b "
      }

      "print(Any)" in new scope {
        new Wok {
          out.print("a")
          out.print("b")
        }
        result mustEqual "ab"
      }
    }

    "add a extended function !<< to String" in {
      import Helpers.ExtendedPathString

      trait scope extends Scope {
        val out = Path.createTempFile().path.!<<
        def result = out.string
      }

      "println()" in new scope {
        new Wok {
          out.println()
          out.println()
        }
        result mustEqual "\n\n"
      }

      "printf()" in new scope {
        new Wok {
          out.printf()
          out.printf()
        }
        result mustEqual "  "
      }

      "print()" in new scope {
        new Wok {
          out.print()
          out.print()
        }
        result mustEqual ""
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
          out.printf("a")
          out.printf("b")
        }
        result mustEqual "a b "
      }

      "print(Any)" in new scope {
        new Wok {
          out.print("a")
          out.print("b")
        }
        result mustEqual "ab"
      }
    }

    "provide functions print/println" in {

      trait scope extends Scope {
        val out = new TestOutputStream
        def result = new String(out.toByteArray, StandardCharsets.UTF_8)
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

      "printf()" in new scope {
        Stdio.withOut(out) {
          new Wok {
            printf()
            printf()
          }
        }
        result mustEqual "  "
      }

      "print()" in new scope {
        Stdio.withOut(out) {
          new Wok {
            print()
            print()
          }
        }
        result mustEqual ""
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

      "print(Any)" in new scope {
        Stdio.withOut(out) {
          new Wok {
            printf("a")
            printf("b")
          }
        }
        result mustEqual "a b "
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
    }

    "ExtendedInputStreamResource" in {
      import Helpers.ExtendedInputStreamResource

      "add a extended function #> to InputStreamResource" in {
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

      "add a extended function #<< to OutputStreamResource" in {
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

      "add a extended function #> to Path" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          val wok = new Wok {
            def string = out #> { _.read().toChar.toString }
          }
          wok.string mustEqual "a"
        }
      }

      "add a extended function #< to Path" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out #< { _.write("b".getBytes) }
          }
          out.string mustEqual "b"
        }
      }

      "add a extended function #<< to Path" in {
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

      "add a extended function #> to String" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          val wok = new Wok {
            def string = out.path #> { _.read().toChar.toString }
          }
          wok.string mustEqual "a"
        }
      }

      "add a extended function #< to String" in {
        "write a Array[Byte]" in {
          val out = Path.createTempFile()
          out.write("a")
          new Wok {
            out.path #< { _.write("b".getBytes) }
          }
          out.string mustEqual "b"
        }
      }

      "add a extended fucntion #<< to String" in {
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
