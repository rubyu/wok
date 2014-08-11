package wok.process

import org.specs2.mutable._
import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}


class ProcessTest extends SpecificationWithJUnit {

  "Proc.escape" should {

    def python(s: String) =
      Seq("python", "-c",
        """import sys
          |if sys.platform == 'win32':
          |  import os, msvcrt
          |  msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
          |sys.stdout.write(sys.argv[1])""".stripMargin, s)
        .!>.string

    def echo(s: String) =
      Seq("echo","-n", s).!>.string

    val patterns = Seq(
      "",
      "a",  //normal character
      " ", "\t", "\n",  //spaces
      "%DUMMY%", "%PATH%",   //windows environment parameters
      "\\", "\"", "'", "^", "<", ">", "|", "[", "]", "&", "%", //cmd.exe's special characters
      ";", "&", "(", ")", "|", "^", "<", ">", "?", "*", "[", "]",
      "$", "$a", "`", "'", "\"", "\\", "!", "{", "}" //bash's special characters
    )

    "escape given parameters" in {
      patterns map { p =>
        python(p) mustEqual p
        echo(p) mustEqual p
      }
    }
  }

  "Proc.exec" should {
    "call a program 1" in {
      "echo -n a"
        .!>.string mustEqual "a"
    }

    "call a program 2" in {
      Seq("echo", "-n", "a")
        .!>.string mustEqual "a"
    }

    "call a program with arguments" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt")
        .!>.string mustEqual "angel\néindʒəl\n"
    }

    "call a program with unicode encoded arguments" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt")
        .#|( Seq("grep", "éindʒəl") )
        .!>.string mustEqual "éindʒəl\n"
    }

    "call a program with standard input" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-")
        .#<("stdin-data").!>.string mustEqual "angel\néindʒəl\nstdin-data"
    }

    "call a program with unicode encoded standard input" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-")
        .#<("éindʒəl").!>.string mustEqual "angel\néindʒəl\néindʒəl"
    }

    "connect programs" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt")
        .#|( Seq("grep", "an") )
        .!>.string mustEqual "angel\n"
    }
  }
}