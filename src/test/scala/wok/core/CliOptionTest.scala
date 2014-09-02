package wok.core

import org.specs2.mutable._

import scalax.file.Path


class CliOptionTest extends SpecificationWithJUnit {

  "CliOption.parse" should {
    "throw AssertionError" in {
      "when parse -v" in {
        CliOption.parse(List("-v")) must throwAn[AssertionError]
      }
      "when parse -v value not containing '='" in {
        CliOption.parse(List("-v", "a")) must throwAn[AssertionError]
      }
      "when parse -v value containing '=', but at last" in {
        CliOption.parse(List("-v", "a=")) must throwAn[AssertionError]
      }
      "when parse -v@" in {
        CliOption.parse(List("-v@")) must throwAn[AssertionError]
      }
      "when parse -v@ value not containing '='" in {
        CliOption.parse(List("-v@", "a")) must throwAn[AssertionError]
      }
      "when parse -v@ value containing '=', but at last" in {
        CliOption.parse(List("-v@", "a=")) must throwAn[AssertionError]
      }
      "when parse -v@non_supported_type value" in {
        CliOption.parse(List("-v@non_supported_type", "a=b")) must throwAn[AssertionError]
      }
    }

    "parse -v a=b" in {
      CliOption.parse(List("-v", "a=b")) mustEqual CliOption(Nil, "var a = b", false)
    }
    "parse -v@str a=b" in {
      CliOption.parse(List("-v@str", "a=b")) mustEqual CliOption(Nil, "var a = \"b\"", false)
    }
    "parse -v@rawstr a=b" in {
      CliOption.parse(List("-v@rawstr", "a=b")) mustEqual CliOption(Nil, "var a = \"\"\"b\"\"\"", false)
    }
    "parse -v@char a=b" in {
      CliOption.parse(List("-v@char", "a=b")) mustEqual CliOption(Nil, "var a = 'b'", false)
    }
    "parse a" in {
      CliOption.parse(List("a")) mustEqual CliOption(Nil, "a", false)
    }
    "parse a b" in {
      CliOption.parse(List("a", "b")) mustEqual CliOption(List("b"), "a", false)
    }
    "parse --" in {
      CliOption.parse(List("--")) mustEqual CliOption(Nil, "", false)
    }
    "parse --diag" in {
      CliOption.parse(List("--diag")) mustEqual CliOption(Nil, "", true)
    }
    "parse -f" in {
      "parse script" in {
        val f1 = Path.createTempFile()
        f1.write(List("a", "b", "c") mkString "\n")
        CliOption.parse(List("-f", f1.path)) mustEqual CliOption(Nil, "a\nb\nc", false)
      }
      "parse scripts and args" in {
        val f1 = Path.createTempFile()
        f1.write("a")
        val f2 = Path.createTempFile()
        f2.write("b")
        CliOption.parse(List("-f", f1.path, "-f", f2.path, "arg1", "arg2")) mustEqual
          CliOption(List("arg1", "arg2"), "a\nb", false)
      }
      "comment out shebang" in {
        val f1 = Path.createTempFile()
        f1.write("#")
        val f2 = Path.createTempFile()
        f2.write("a")
        CliOption.parse(List("-f", f1.path, "-f", f2.path)) mustEqual
          CliOption(Nil, "//#\na", false)
      }
    }
    "parse -- a" in {
      CliOption.parse(List("--", "a")) mustEqual CliOption(List("a"), "", false)
    }
  }
}