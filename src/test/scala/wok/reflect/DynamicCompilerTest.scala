package wok.reflect

import java.nio.charset.StandardCharsets

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.Helpers._
import wok.core.Stdio


class DynamicCompilerTest extends SpecificationWithJUnit {
  "DynamicCompiler.sourceString" should {
    "gerarate pretty formatted Wok.scala" in {
      "when empty data given" in {
        DynamicCompiler.sourceString(Nil, None, Nil) mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
            "import wok.csv.{Quote, Reader, Row, Writer}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(val args: List[String]) extends AbstractWok {",
            "  def runScript(): Unit = {}",
            "}"
          ).mkString("\n")
      }

      "when before given" in {
        DynamicCompiler.sourceString(List("a", "b"), None, Nil) mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
            "import wok.csv.{Quote, Reader, Row, Writer}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(val args: List[String]) extends AbstractWok {",
            "  def runScript(): Unit = {",
            "    a ;",
            "    b ;",
            "  }",
            "}"
          ).mkString("\n")
      }

      "when script given" in {
        DynamicCompiler.sourceString(Nil, Some("a"), Nil) mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
            "import wok.csv.{Quote, Reader, Row, Writer}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(val args: List[String]) extends AbstractWok {",
            "  def runScript(): Unit = {",
            "    {",
            "      var currentRow = Row(0, Nil, Nil, \"\", \"\")",
            "      def NF = currentRow.size",
            "      def NR = currentRow.id",
            "      def FT = currentRow.sep",
            "      def RT = currentRow.term",
            "      STDIN #> {",
            "        _.csv.map { row => currentRow = row; row } a",
            "      }",
            "    }",
            "  }",
            "}"
          ).mkString("\n")
      }

      "when before given" in {
        DynamicCompiler.sourceString(Nil, None, List("a", "b")) mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
            "import wok.csv.{Quote, Reader, Row, Writer}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(val args: List[String]) extends AbstractWok {",
            "  def runScript(): Unit = {",
            "    a ;",
            "    b ;",
            "  }",
            "}"
          ).mkString("\n")
      }

      "when all the data given" in {
        DynamicCompiler.sourceString(List("a", "b"), Some("c"), List("d", "e")) mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
            "import wok.csv.{Quote, Reader, Row, Writer}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(val args: List[String]) extends AbstractWok {",
            "  def runScript(): Unit = {",
            "    a ;",
            "    b ;",
            "    {",
            "      var currentRow = Row(0, Nil, Nil, \"\", \"\")",
            "      def NF = currentRow.size",
            "      def NR = currentRow.id",
            "      def FT = currentRow.sep",
            "      def RT = currentRow.term",
            "      STDIN #> {",
            "        _.csv.map { row => currentRow = row; row } c",
            "      }",
            "    }",
            "    d ;",
            "    e ;",
            "  }",
            "}"
          ).mkString("\n")
      }
    }
  }

  "DynamicCompiler.compile" should {

    sequential

    trait scope extends Scope {
      val out = new TestOutputStream
      val err = new TestOutputStream
    }

    "provide implicit scalax.io.OutputConverter" in {
      "" in new scope {
        "ByteArrayConverter" in new scope {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("STDOUT.write(\"a\".getBytes)"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
          out.toString mustEqual "a"
        }
      }
    }

    "provide implicit conversions" in {
      "to Charset" in {
        "codecToCharset" in new scope {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("val c = Codec(\"Windows-31J\"); print(new String(\"あ\".getBytes(c), c))"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
          out.toString("utf-8") mustEqual "あ"
        }
      }

      "to Process" in {
        "stringSeq2process" in new scope {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("print(Seq(\"echo\", \"-n\", \"a\").!>.string)"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
          out.toString mustEqual "a"
        }

        "string2process" in new scope {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("print(\"echo -n a\".!>.string)"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
          out.toString mustEqual "a"
        }
      }

      "to Path" in {
        "string2path" in new scope {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("STDOUT.write(\"a\".name)"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
          out.toString mustEqual "a"
        }
      }
    }

    "provide access to" in {
      "Row" in new scope {
        Stdio.withIn(new TestInputStream("a b c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(Nil, Some("foreach { row => print(row: _*) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "a b c"
      }

      "NF" in new scope {
        Stdio.withIn(new TestInputStream("a b c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(Nil, Some("foreach { row => print(NF) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "3"
      }

      "NR" in new scope {
        Stdio.withIn(new TestInputStream("a b c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(Nil, Some("foreach { row => print(NR) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "0"
      }

      "FT" in new scope {
        Stdio.withIn(new TestInputStream("a b c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("OFQ(Quote() Min())"), Some("foreach { row => print(FT: _*) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "\" \" \" \""
      }

      "RT" in new scope {
        Stdio.withIn(new TestInputStream("a b c\n")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("OFQ(Quote() Min())"), Some("foreach { row => print(RT) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "\"\n\""
      }

      "args" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile(List("print(args: _*)"), None, Nil)
            ._2.create(List("1", "2", "3"))
            .runScript()
        }
        out.toString mustEqual "1 2 3"
      }

      "Quote" in new scope {
        Stdio.withIn(new TestInputStream("a\\ b\\ c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("FQ(Quote() None() E('\\\\'))", "OFQ(FQ)"), Some("foreach { row => print(row: _*) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "a\\ b\\ c"
      }

      "Reader" in new scope {
        Stdio.withIn(new TestInputStream("a b c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("STDIN #> { _.csv(Reader()) foreach { row => print(row: _*) } }"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "a b c"
      }

      "Writer" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile(List("print(\"a\")"), None, Nil)
            ._2.create(Nil)
            .runScript()
        }
        out.toString mustEqual "a"
      }

      "Codec" in new scope {
        Stdio.withIn(new TestInputStream("あ", "Windows-31J")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("CD(Codec(\"Windows-31J\"))"), Some("foreach { row => print(row: _*) }"), Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString("utf-8") mustEqual "あ"
      }

      "Path" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile(List("val p = Path(\"test\"); p.write(\"a\"); print(p.string); p.delete()"), None, Nil)
            ._2.create(Nil)
            .runScript()
        }
        out.toString mustEqual "a"
      }

      "Resource" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile(List("val out = Resource.fromFile(\"test\"); out.write(\"a\"); print(out.string); Path(\"test\").delete()"), None, Nil)
            ._2.create(Nil)
            .runScript()
        }
        out.toString mustEqual "a"
      }

      "STDIN" in new scope {
        Stdio.withIn(new TestInputStream("あ")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile(List("STDOUT.write(STDIN.string)"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "あ"
      }

      "STDERR" in new scope {
        Stdio.withIn(new TestInputStream("あ")) {
          Stdio.withErr(err) {
            DynamicCompiler
              .compile(List("STDERR.write(STDIN.string)"), None, Nil)
              ._2.create(Nil)
              .runScript()
          }
        }
        err.toString mustEqual "あ"
      }
    }
  }
}
