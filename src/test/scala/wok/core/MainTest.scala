package wok.core

import org.specs2.mutable._
import org.specs2.specification.Scope
import wok.Helpers._


class MainTest extends SpecificationWithJUnit {

  "Main.main" should {

    sequential

    trait scope extends Scope with After {
      val _sm = System.getSecurityManager
      System.setSecurityManager(new MockExitSecurityManager)
      def after {
        System.setSecurityManager(_sm)
      }
    }

    "execute script" in new scope {
      val in = new TestInputStream("a b c")
      val out = new TestOutputStream()
      Stdio.withIn(in) {
        Stdio.withOut(out) {
          Main.main(Array("In { _ foreach { row => print(row: _*) }}"))
        }
      }
      out.toString mustEqual "a b c"
    }

    "do diagnosis with no error" in new scope {
      val out = new TestOutputStream()
      Console.withOut(out) {
        Main.main(Array("--diag"))
      }
      out.toString mustEqual
        List(
          "The results of diagnosis are:",
          "No errors and warning found.",
          "",
          "| 1| package wok",
          "| 2| ",
          "| 3| import wok.reflect.AbstractWok",
          "| 4| import wok.reflect.Helpers._",
          "| 5| import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}",
          "| 6| import wok.csv.{Quote, Row}",
          "| 7| import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
          "| 8| import scalax.io.{Codec, Resource}",
          "| 9| import scalax.file.Path",
          "|10| import scalax.file.ImplicitConversions.string2path",
          "|11| ",
          "|12| class Wok(override val args: List[String]) extends AbstractWok {",
          "|13| def runScript(): Unit = {",
          "|14| ",
          "|15| }}",
          ""
        ).mkString(System.lineSeparator())
    }

    "do diagnosis with error" in new scope {
      val out = new TestOutputStream()
      Console.withOut(out) {
        Main.main(Array("--diag", "foo"))
      }
      out.toString mustEqual
        List(
          "The results of diagnosis are:",
          "Errors: 1, Warning: 0",
          "",
          "14: ERROR: not found: value foo",
          "foo",
          "^",
          "",
          "| 1| package wok",
          "| 2| ",
          "| 3| import wok.reflect.AbstractWok",
          "| 4| import wok.reflect.Helpers._",
          "| 5| import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}",
          "| 6| import wok.csv.{Quote, Row}",
          "| 7| import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
          "| 8| import scalax.io.{Codec, Resource}",
          "| 9| import scalax.file.Path",
          "|10| import scalax.file.ImplicitConversions.string2path",
          "|11| ",
          "|12| class Wok(override val args: List[String]) extends AbstractWok {",
          "|13| def runScript(): Unit = {",
          "|14| foo",
          "|15| }}",
          ""
        ).mkString(System.lineSeparator())
    }

    "print error messages to Console.err" in new scope {
      val out = new TestOutputStream()
      val in = new TestInputStream("a")
      Stdio.withIn(in) {
        Console.withErr(out) {
          Main.main(Array("foo")) must throwAn(new AttemptToExitException(1))
        }
      }
      out.toString mustEqual
        List(
          "Compilation failed. The details are:",
          "Errors: 1, Warning: 0",
          "",
          "14: ERROR: not found: value foo",
          "foo",
          "^",
          "",
          "| 1| package wok",
          "| 2| ",
          "| 3| import wok.reflect.AbstractWok",
          "| 4| import wok.reflect.Helpers._",
          "| 5| import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}",
          "| 6| import wok.csv.{Quote, Row}",
          "| 7| import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
          "| 8| import scalax.io.{Codec, Resource}",
          "| 9| import scalax.file.Path",
          "|10| import scalax.file.ImplicitConversions.string2path",
          "|11| ",
          "|12| class Wok(override val args: List[String]) extends AbstractWok {",
          "|13| def runScript(): Unit = {",
          "|14| foo",
          "|15| }}",
          ""
      ).mkString(System.lineSeparator())
    }
  }
}