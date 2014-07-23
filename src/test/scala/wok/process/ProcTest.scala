
package wok.process

import org.specs2.mutable._
import Helpers._


class ProcTest extends SpecificationWithJUnit {

  "Proc.escape" should {
    def python(s: String) =
      new Proc(Seq("python", "-c",  "\"import sys; sys.stdout.write(sys.argv[1])\"", s)).exec.string

    def echo(s: String) =
      new Proc(Seq("echo", s)).exec.string.dropRight(1)

    val patterns = Seq(
      "",
      "a",  //normal character
      " ", "\t",  //spaces
      "%DUMMY%", "%PATH%",   //windows environment parameters
      "\\", "\"", "'", "^", "<", ">", "|", "[", "]", "&", "%" //windows cmd.exe's special characters
    )

    "escape given parameters" in {
      patterns map { p =>
        python(Proc.escape(p)) mustEqual p
        echo(Proc.escape(p)) mustEqual p
      }
    }
  }

  "Proc.exec" should {
    "throw IllegalArgumentException when commandStrings is '|'" in {
      Seq("|")
        .exec must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings starts with '|'" in {
      Seq("|", "a")
        .exec must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings ends with '|'" in {
      Seq("a", "|")
        .exec must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings is empty" in {
      Seq()
        .exec must throwAn[IllegalArgumentException]
    }

    "call a program" in {
      Seq("echo", "-n", "a")
        .exec.string mustEqual "a"
    }

    "call a program with Int argunent" in {
      Seq("echo", "-n", 1)
        .exec.string mustEqual "1"
    }

    "call a program with arguments" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt")
        .exec.string mustEqual "angel\r\néindʒəl\r\n"
    }

    "call a program with unicode encoded arguments" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt",
        "|", "grep", "éindʒəl")
        .exec.string mustEqual "éindʒəl\n"
    }

    "call a program with standard input" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-")
        .exec("stdin-data").string mustEqual "angel\r\néindʒəl\r\nstdin-data"
    }

    "call a program with unicode encoded standard input" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-")
        .exec("éindʒəl").string mustEqual "angel\r\néindʒəl\r\néindʒəl"
    }

    "connect programs" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt",
        "|", "grep", "an")
        .exec.string mustEqual "angel\n"
    }
  }
}
