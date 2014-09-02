
package wok.reflect

import java.nio.charset.StandardCharsets

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.Helpers._
import wok.csv.Quote
import wok.core.Stdio
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}
import scalax.file.Path
import scalax.io.{StandardOpenOption, Codec}


class AbstractWokTest extends SpecificationWithJUnit {

  "ThreadSafeVariables" should {
    "pass value to child thread" in {
      val wok = new AbstractWok {
        def runScript() {}
        def value = Future { FILENAME }
      }
      val v1 = wok.FILENAME
      val v2 = Await.result(wok.value, Duration(1, SECONDS))
      v1 eq v2 must beTrue
    }
    "pass value to child thread when a change given" in {
      val wok = new AbstractWok {
        def runScript() {}
        def value = Future { FILENAME }
      }
      wok._FILENAME.value = "a"
      val v1 = wok.FILENAME
      val v2 = Await.result(wok.value, Duration(1, SECONDS))
      v1 eq v2 must beTrue
    }
    "pass copied value to chid thread" in {
      val wok = new AbstractWok {
        def runScript() {}
        def value = Future { READER }
      }
      val v1 = wok.READER
      val v2 = Await.result(wok.value, Duration(1, SECONDS))
      v1 ne v2 must beTrue
    }
  }

  "AbstractWok.In" should {
    "read data from Stdin when no argument given" in {
      Stdio.withIn(new TestInputStream("a b c")) {
        val wok = new AbstractWok {
          def runScript(){}
          def in = In { _ map (_ mkString) mkString }
        }
        wok.in mustEqual "abc"
      }
    }
    "read data from files when arguments given" in {
      val f1 = Path.createTempFile()
      f1.write("a b c")
      val f2 = Path.createTempFile()
      f2.write("d e f")
      val wok = new AbstractWok {
        override val args = List(f1.path, f2.path)
        def runScript(){}
        def in = In { _ map (_ mkString) mkString }
      }
      wok.in mustEqual "abcdef"
    }
  }

  "PathStringInputProcessor.process" should {
    val p1 = Path.createTempFile()
    p1.write("a b c")
    val p2 = Path.createTempFile()
    p2.write("d e f")
    "protect inherited mutable variables" in {
      val wok = new AbstractWok {
        def runScript(){}
        def in = In.from(p1.path, p2.path) {
          _ map { row =>
            FS = "="
            OFS = "="
            (row, FS, OFS)
          } toList
        }
      }
      val result = wok.in

      {
        val (row, fs, ofs) = result(0)
        row.size mustEqual 3
        fs.toString mustEqual "="
        ofs.toString mustEqual "="
      }

      {
        val (row, fs, ofs) = result(1)
        row.size mustEqual 1
        fs.toString mustEqual "="
        ofs.toString mustEqual "="
      }

      wok.FS.toString mustEqual "[ \\t]+"
      wok.OFS.toString mustEqual " "
    }
    "provide access to Reader's property" in {
      val wok = new AbstractWok {
        def runScript(){}
        def in = In.from(p1.path, p2.path) {
          _ map { row =>
            try {
              (row, FS)
            }
            finally FS = "="
          } toList
        }
      }
      val result = wok.in

      {
        val (row, fs) = result(0)
        row.size mustEqual 3
        fs.toString mustEqual "[ \\t]+"
      }

      {
        val (row, fs) = result(1)
        row.size mustEqual 1
        fs.toString mustEqual "="
      }
    }
    "provide immutable variables" in {
      val wok = new AbstractWok {
        def runScript(){}
        def in = In.from(p1.path, p2.path) {
          _ map { row =>
            (row, ARGV, ARGC, ARGIND, FILENAME, FNR, NR, NF, FT, RT, $0)
          } toList
        }
      }
      val result = wok.in
      result.size mustEqual 2

      {
        val (row, argv, argc, argind, filename, fnr, nr, nf, ft, rt, source) = result(0)
        row.size mustEqual 3
        argv mustEqual List(p1.path, p2.path)
        argc mustEqual 2
        argind mustEqual 0
        filename mustEqual p1.path
        fnr mustEqual 0
        nr mustEqual 0
        nf mustEqual 3
        ft mustEqual List(" ", " ")
        rt mustEqual ""
        source mustEqual "a b c"
      }

      {
        val (row, argv, argc, argind, filename, fnr, nr, nf, ft, rt, source) = result(1)
        row.size mustEqual 3
        argv mustEqual List(p1.path, p2.path)
        argc mustEqual 2
        argind mustEqual 1
        filename mustEqual p2.path
        fnr mustEqual 0
        nr mustEqual 1
        nf mustEqual 3
        ft mustEqual List(" ", " ")
        rt mustEqual ""
        source mustEqual "d e f"
      }
    }
  }

  "StreamInputProcessor.process" should {
    "protect inherited mutable variables" in {
      val s1 = new TestInputStream("a b c")
      val s2 = new TestInputStream("d e f")
      val wok = new AbstractWok {
        def runScript(){}
        def in = In.from(s1, s2) {
          _ map { row =>
            FS = "="
            OFS = "="
            (row, FS, OFS)
          } toList
        }
      }
      val result = wok.in

      {
        val (row, fs, ofs) = result(0)
        row.size mustEqual 3
        fs.toString mustEqual "="
        ofs.toString mustEqual "="
      }

      {
        val (row, fs, ofs) = result(1)
        row.size mustEqual 1
        fs.toString mustEqual "="
        ofs.toString mustEqual "="
      }

      wok.FS.toString mustEqual "[ \\t]+"
      wok.OFS.toString mustEqual " "
    }
    "provide access to Reader's property" in {
      val s1 = new TestInputStream("a b c")
      val s2 = new TestInputStream("d e f")
      val wok = new AbstractWok {
        def runScript(){}
        def in = In.from(s1, s2) {
          _ map { row =>
            try {
              (row, FS)
            }
            finally FS = "="
          } toList
        }
      }
      val result = wok.in

      {
        val (row, fs) = result(0)
        row.size mustEqual 3
        fs.toString mustEqual "[ \\t]+"
      }

      {
        val (row, fs) = result(1)
        row.size mustEqual 1
        fs.toString mustEqual "="
      }
    }
    "provide immutable variables" in {
      val s1 = new TestInputStream("a b c")
      val s2 = new TestInputStream("d e f")
      val wok = new AbstractWok {
        def runScript(){}
        def in = In.from(s1, s2) {
          _ map { row =>
            (row, ARGV, ARGC, ARGIND, FILENAME, FNR, NR, NF, FT, RT, $0)
          } toList
        }
      }
      val result = wok.in
      result.size mustEqual 2

      {
        val (row, argv, argc, argind, filename, fnr, nr, nf, ft, rt, source) = result(0)
        row.size mustEqual 3
        argv mustEqual List("-", "-")
        argc mustEqual 2
        argind mustEqual 0
        filename mustEqual "-"
        fnr mustEqual 0
        nr mustEqual 0
        nf mustEqual 3
        ft mustEqual List(" ", " ")
        rt mustEqual ""
        source mustEqual "a b c"
      }

      {
        val (row, argv, argc, argind, filename, fnr, nr, nf, ft, rt, source) = result(1)
        row.size mustEqual 3
        argv mustEqual List("-", "-")
        argc mustEqual 2
        argind mustEqual 1
        filename mustEqual "-"
        fnr mustEqual 0
        nr mustEqual 1
        nf mustEqual 3
        ft mustEqual List(" ", " ")
        rt mustEqual ""
        source mustEqual "d e f"
      }
    }
  }

  "AbstractWok" should {

    class Wok extends AbstractWok {
      def runScript(){}
    }

    "support accesses for Reader's parameters" in {
      "FS" in {
        val wok = new Wok()
        wok.FS.toString mustEqual "[ \\t]+"
        wok.FS = "a".r
        wok.FS.toString mustEqual "a"
        wok.FS = "a"
        wok.FS.toString mustEqual "a"
        wok.FS = ""
        wok.FS.toString mustEqual "(?!.)."
        wok.FS = 'a'
        wok.FS.toString mustEqual "a"
      }

      "RS" in {
        val wok = new Wok()
        wok.RS.toString mustEqual "\\r\\n|\\r|\\n"
        wok.RS = "a".r
        wok.RS.toString mustEqual "a"
        wok.RS = "a"
        wok.RS.toString mustEqual "a"
        wok.RS = ""
        wok.RS.toString mustEqual "(?!.)."
        wok.RS = 'a'
        wok.RS.toString mustEqual "a"
      }

      "FQ" in {
        val wok = new Wok()
        wok.FQ mustEqual Quote.None()
        wok.FQ = Quote All()
        wok.FQ mustEqual Quote.All()
      }

      "CD" in {
        val wok = new Wok()
        wok.CD mustEqual Codec.default
        wok.CD = Codec.ISO8859
        wok.CD mustEqual Codec.ISO8859
      }
    }

    "support accesses for Writer's parameters" in {
      "OFS" in {
        val wok = new Wok()
        wok.OFS mustEqual " "
        wok.OFS = "a"
        wok.OFS mustEqual "a"
        wok.OFS = 'a'
        wok.OFS mustEqual "a"
      }

      "ORS" in {
        val wok = new Wok()
        wok.ORS mustEqual "\n"
        wok.ORS = "a"
        wok.ORS mustEqual "a"
        wok.ORS = 'a'
        wok.ORS mustEqual "a"
      }

      "OFQ" in {
        val wok = new Wok()
        wok.OFQ mustEqual Quote.None()
        wok.OFQ = Quote All()
        wok.OFQ mustEqual Quote.All()
      }

      "OCD" in {
        val wok = new Wok()
        wok.OCD mustEqual Codec.default
        wok.OCD = Codec.ISO8859
        wok.OCD mustEqual Codec.ISO8859
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
