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
        DynamicCompiler.sourceString("") mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}",
            "import wok.csv.{Quote, Row}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(override val args: List[String]) extends AbstractWok {",
            "def runScript(): Unit = {",
            "",
            "}}",
            ""
          ).mkString("\n")
      }

      "when script given" in {
        DynamicCompiler.sourceString("a") mustEqual
          List(
            "package wok",
            "",
            "import wok.reflect.AbstractWok",
            "import wok.reflect.Helpers._",
            "import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}",
            "import wok.csv.{Quote, Row}",
            "import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
            "import scalax.io.{Codec, Resource}",
            "import scalax.file.Path",
            "import scalax.file.ImplicitConversions.string2path",
            "",
            "class Wok(override val args: List[String]) extends AbstractWok {",
            "def runScript(): Unit = {",
            "a",
            "}}",
            ""
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
              .compile("STDOUT.write(\"a\".getBytes)")
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
              .compile("val c = Codec(\"Windows-31J\"); print(new String(\"あ\".getBytes(c), c))")
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
              .compile("print(Seq(\"echo\", \"-n\", \"a\").!>.string)")
              ._2.create(Nil)
              .runScript()
          }
          out.toString mustEqual "a"
        }

        "string2process" in new scope {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile("print(\"echo -n a\".!>.string)")
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
              .compile("Stdout.write(\"a\".name)")
              ._2.create(Nil)
              .runScript()
          }
          out.toString mustEqual "a"
        }
      }
    }

    "provide access to" in {
      "In" in new scope {
        Stdio.withIn(new TestInputStream("a b c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile("In { _ foreach { row => print(row: _*) }}")
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
              .compile("In { _ foreach { row => print(NF) }}")
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
              .compile("In { _ foreach { row => print(NR) }}")
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
              .compile("OFQ(Quote() Min()); In { _ foreach { row => print(FT: _*) }}")
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
              .compile("OFQ(Quote() Min()); In { _ foreach { row => print(RT) }}")
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "\"\n\""
      }

      "args" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile("print(args: _*)")
            ._2.create(List("1", "2", "3"))
            .runScript()
        }
        out.toString mustEqual "1 2 3"
      }

      "Quote" in new scope {
        Stdio.withIn(new TestInputStream("a\\ b\\ c")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile("FQ(Quote() None() E('\\\\')); OFQ(FQ); In { _ foreach { row => print(row: _*) }}")
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "a\\ b\\ c"
      }

      "Codec" in new scope {
        Stdio.withIn(new TestInputStream("あ", "Windows-31J")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile("CD(Codec(\"Windows-31J\")); In { _ foreach { row => print(row: _*) }}")
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString("utf-8") mustEqual "あ"
      }

      "Path" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile("val p = Path(\"test\"); p.write(\"a\"); print(p.string); p.delete()")
            ._2.create(Nil)
            .runScript()
        }
        out.toString mustEqual "a"
      }

      "Resource" in new scope {
        Stdio.withOut(out) {
          DynamicCompiler
            .compile("val out = Resource.fromFile(\"test\"); out.write(\"a\"); print(out.string); Path(\"test\").delete()")
            ._2.create(Nil)
            .runScript()
        }
        out.toString mustEqual "a"
      }

      "Stdin" in new scope {
        Stdio.withIn(new TestInputStream("あ")) {
          Stdio.withOut(out) {
            DynamicCompiler
              .compile("Stdout.write(Stdin.string)")
              ._2.create(Nil)
              .runScript()
          }
        }
        out.toString mustEqual "あ"
      }

      "Stderr" in new scope {
        Stdio.withIn(new TestInputStream("あ")) {
          Stdio.withErr(err) {
            DynamicCompiler
              .compile("Stderr.write(Stdin.string)")
              ._2.create(Nil)
              .runScript()
          }
        }
        err.toString mustEqual "あ"
      }
    }
  }
}
