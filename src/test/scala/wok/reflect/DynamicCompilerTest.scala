package wok.reflect

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.core.Stdio
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}


class DynamicCompilerTest extends SpecificationWithJUnit {
  "DynamicCompiler" should {

    sequential

    trait scope extends Scope {
      val out = new ByteArrayOutputStream
      val err = new ByteArrayOutputStream
    }

    implicit def stringToByteArrayInputStream(s: String) = new ByteArrayInputStream(s.getBytes("utf-8"))
    implicit def bytesToByteArrayInputStream(bytes: Array[Byte]) = new ByteArrayInputStream(bytes)

    "compile Wok" should {
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
          Stdio.withIn("a b c") {
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
          Stdio.withIn("a b c") {
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
          Stdio.withIn("a b c") {
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
          Stdio.withIn("a b c") {
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
          Stdio.withIn("a b c\n") {
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
          Stdio.withIn("a\\ b\\ c") {
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
          Stdio.withIn("a b c") {
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
          Stdio.withIn("あ".getBytes("Windows-31J")) {
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
          Stdio.withIn("あ") {
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
          Stdio.withIn("あ") {
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
}
