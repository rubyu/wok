
package wok.process

import org.specs2.mutable._
import Helpers._


class HelpersTest extends SpecificationWithJUnit {
  "Helpers.ProcessArgumentString.escape" should {

    def python(s: String) =
      new Process(Seq("python", "-c",  "\"import sys; sys.stdout.write(sys.argv[1])\"", s)).exec.string

    def echo(s: String) =
      new Process(Seq("echo", s)).exec.string.dropRight(1)

    val patterns = Seq(
      "",
      "a",  //normal character
      " ", "\t",  //spaces
      "%DUMMY%", "%PATH%",   //windows environment parameters
      "\\", "\"", "'", "^", "<", ">", "|", "[", "]", "&", "%" //windows cmd.exe's special characters
    )

    "quote given strings" in {
      patterns map { p =>
        python(p.escaped) mustEqual p
        echo(p.escaped) mustEqual p
      }
    }
  }
}