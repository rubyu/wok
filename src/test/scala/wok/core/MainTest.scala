package wok.core

import java.io.ByteArrayOutputStream

import org.specs2.mutable._
import org.specs2.specification.Scope


class MainTest extends SpecificationWithJUnit {

  "Main.main" should {

    sequential

    class AttemptToExitException(val status: Int) extends RuntimeException

    class MockExitSecurityManager extends java.rmi.RMISecurityManager {
      override def checkExit(status: Int) { throw new AttemptToExitException(status) }
      override def checkPermission(perm: java.security.Permission) {}
    }

    trait scope extends Scope with After {
      val _sm = System.getSecurityManager
      System.setSecurityManager(new MockExitSecurityManager)
      def after {
        System.setSecurityManager(_sm)
      }
    }

    "execute a script of Wok" in new scope {
      val out = new ByteArrayOutputStream()
      Stdio.withOut(out) {
        Main.main(Array("-b" ,"print(\"a\")"))
      }
      out.toString mustEqual "a"
    }

    "do diagnosis with no error" in new scope {
      val out = new ByteArrayOutputStream()
      Console.withOut(out) {
        Main.main(Array("--diag"))
      }
      out.toString mustEqual
        List(
          "The results of diagnosis are:",
          "No errors and warning found.",
          "",
          "| 1| ",
          "| 2| package wok",
          "| 3| ",
          "| 4| import wok.reflect.AbstractWok",
          "| 5| import wok.reflect.Helpers._",
          "| 6| import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
          "| 7| import wok.csv.{Quote, Reader, Row, Writer}",
          "| 8| import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
          "| 9| import scalax.io.{Codec, Resource}",
          "|10| import scalax.file.Path",
          "|11| import scalax.file.ImplicitConversions.string2path",
          "|12| ",
          "|13| ",
          "|14| class Wok(val args: List[String]) extends AbstractWok {",
          "|15|   def runScript(): Unit = {",
          "|16| ",
          "|17| ",
          "|18| ",
          "|19| // no script",
          "|20| ",
          "|21| ",
          "|22| ",
          "|23|   }",
          "|24| }",
          ""
        ).mkString(System.lineSeparator())
    }

    "do diagnosis with error" in new scope {
      val out = new ByteArrayOutputStream()
      Console.withOut(out) {
        Main.main(Array("--diag", "-b" ,"nonExistentIdentifier"))
      }
      out.toString mustEqual
        List(
          "The results of diagnosis are:",
          "Errors: 1, Warning: 0",
          "",
          "17: ERROR: not found: value nonExistentIdentifier",
          "    nonExistentIdentifier",
          "    ^",
          "",
          "| 1| ",
          "| 2| package wok",
          "| 3| ",
          "| 4| import wok.reflect.AbstractWok",
          "| 5| import wok.reflect.Helpers._",
          "| 6| import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
          "| 7| import wok.csv.{Quote, Reader, Row, Writer}",
          "| 8| import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
          "| 9| import scalax.io.{Codec, Resource}",
          "|10| import scalax.file.Path",
          "|11| import scalax.file.ImplicitConversions.string2path",
          "|12| ",
          "|13| ",
          "|14| class Wok(val args: List[String]) extends AbstractWok {",
          "|15|   def runScript(): Unit = {",
          "|16| ",
          "|17|     nonExistentIdentifier",
          "|18| ",
          "|19| // no script",
          "|20| ",
          "|21| ",
          "|22| ",
          "|23|   }",
          "|24| }",
          ""
        ).mkString(System.lineSeparator())
    }

    "print error messages to Console.err" in new scope {
      val out = new ByteArrayOutputStream()
      Console.withErr(out) {
        Main.main(Array("-b" ,"nonExistentIdentifier")) must throwAn(new AttemptToExitException(1))
      }
      out.toString mustEqual
        List(
          "Compilation failed. The details are:",
          "Errors: 1, Warning: 0",
          "",
          "17: ERROR: not found: value nonExistentIdentifier",
          "    nonExistentIdentifier",
          "    ^",
          "",
          "| 1| ",
          "| 2| package wok",
          "| 3| ",
          "| 4| import wok.reflect.AbstractWok",
          "| 5| import wok.reflect.Helpers._",
          "| 6| import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}",
          "| 7| import wok.csv.{Quote, Reader, Row, Writer}",
          "| 8| import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}",
          "| 9| import scalax.io.{Codec, Resource}",
          "|10| import scalax.file.Path",
          "|11| import scalax.file.ImplicitConversions.string2path",
          "|12| ",
          "|13| ",
          "|14| class Wok(val args: List[String]) extends AbstractWok {",
          "|15|   def runScript(): Unit = {",
          "|16| ",
          "|17|     nonExistentIdentifier",
          "|18| ",
          "|19| // no script",
          "|20| ",
          "|21| ",
          "|22| ",
          "|23|   }",
          "|24| }",
          ""
      ).mkString(System.lineSeparator())
    }
  }
}