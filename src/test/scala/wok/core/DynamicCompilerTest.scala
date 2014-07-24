package wok.reflect

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.core.SystemInput
import java.io.{ByteArrayOutputStream, PrintStream, BufferedOutputStream, ByteArrayInputStream}


class DynamicCompilerTest extends SpecificationWithJUnit {
  "DynamicCompiler" should {

    sequential

    trait scope extends Scope {
      val outStream = new ByteArrayOutputStream
      val out = new PrintStream(new BufferedOutputStream(outStream), true, "utf-8")
      def result = new String(outStream.toByteArray, "utf-8")
    }

    "compile Wok" should {
      "provide implicit conversions" in {
        "codecToCharset" in new scope {
          Console.withOut(out) {
            DynamicCompiler
              .compile(List("val c = Codec(\"Windows-31J\"); print(new String(\"あ\".getBytes(c), c))"), None, Nil)
              .create(Nil)
              .runScript()
          }
          result mustEqual "あ"
        }

        "seq2process" in new scope {
          Console.withOut(out) {
            DynamicCompiler
              .compile(List("print(Seq(\"echo\", \"-n\", \"a\").exec().string)"), None, Nil)
              .create(Nil)
              .runScript()
          }
          result mustEqual "a"
        }
      }

      "provide access to" in {
        "Row" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a b c".getBytes)) {
              DynamicCompiler
                .compile(Nil, Some("foreach { row => print(row) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "a b c"
        }

        "NF" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a b c".getBytes)) {
              DynamicCompiler
                .compile(Nil, Some("foreach { row => print(NF) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "3"
        }

        "NR" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a b c".getBytes)) {
              DynamicCompiler
                .compile(Nil, Some("foreach { row => print(NR) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "0"
        }

        "FT" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a b c".getBytes)) {
              DynamicCompiler
                .compile(List("OFQ(Quote() Min())"), Some("foreach { row => print(FT) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "\" \" \" \""
        }

        "RT" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a b c\n".getBytes)) {
              DynamicCompiler
                .compile(List("OFQ(Quote() Min())"), Some("foreach { row => print(RT) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "\"\n\""
        }

        "arg" in new scope {
          Console.withOut(out) {
            DynamicCompiler
              .compile(List("print(arg)"), None, Nil)
              .create(List("1", "2", "3"))
              .runScript()
          }
          result mustEqual "1 2 3"
        }

        "Quote" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a\\ b\\ c".getBytes)) {
              DynamicCompiler
                .compile(List("FQ(Quote() None() E('\\\\'))", "OFQ(FQ)"), Some("foreach { row => print(row) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "a\\ b\\ c"
        }

        "Reader" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("a b c".getBytes)) {
              DynamicCompiler
                .compile(List("SystemInput.get.open()(Reader()) foreach { row => print(row) }"), None, Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "a b c"
        }

        "Writer" in new scope {
          Console.withOut(out) {
            DynamicCompiler
              .compile(List("print(\"a\")"), None, Nil)
              .create(Nil)
              .runScript()
          }
          result mustEqual "a"
        }

        "Codec" in new scope {
          Console.withOut(out) {
            SystemInput.withValue(new ByteArrayInputStream("あ".getBytes("Windows-31J"))) {
              DynamicCompiler
                .compile(List("CD(Codec(\"Windows-31J\"))"), Some("foreach { row => print(row) }"), Nil)
                .create(Nil)
                .runScript()
            }
          }
          result mustEqual "あ"
        }

        "Path" in new scope {
          Console.withOut(out) {
            DynamicCompiler
              .compile(List("val p = Path(\"test\"); p.write(\"a\"); print(p.string); p.delete()"), None, Nil)
              .create(Nil)
              .runScript()
          }
          result mustEqual "a"
        }

        "Resource" in new scope {
          Console.withOut(out) {
            DynamicCompiler
              .compile(List("val out = Resource.fromFile(\"test\"); out.write(\"a\"); print(out.string); Path(\"test\").delete()"), None, Nil)
              .create(Nil)
              .runScript()
          }
          result mustEqual "a"
        }
      }
    }
  }
}
