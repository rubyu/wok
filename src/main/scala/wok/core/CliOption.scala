package wok.core

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}


case class CliOption(arg: List[String], before: List[String], process: Option[String], after: List[String])


object CliOption {

  def parse(arg: List[String]) = {
    var i = 0
    val before, after = ListBuffer[String]()
    var process: Option[String] = None

    breakable {
      while(true) {
        if (arg.size == i) break
        arg(i) match {
          case "-v" =>
            assume(arg.size> i + 1, "'-v' option requires a argument")
            val a1 = arg(i+1)
            val idx = a1.indexOf("=")
            assume(idx != -1)
            assume(idx != a1.size - 1)
            before.append(s"var ${a1.take(idx)} = ${a1.drop(idx+1)}")
            i += 2
          case a0 if a0.startsWith("-v@") =>
            assume(arg.size> i + 1, s"'-v@type' option requires a argument")
            val a1 = arg(i+1)
            val idx = a1.indexOf("=")
            assume(idx != -1)
            assume(idx != a1.size - 1)
            before.append(a0.drop(3) match {
              case "str"    => s"""var ${a1.take(idx)} = \"${a1.drop(idx+1)}\""""
              case "rawstr" => s"""var ${a1.take(idx)} = \"\"\"${a1.drop(idx+1)}\"\"\""""
              case "char"   => s"""var ${a1.take(idx)} = '${a1.drop(idx+1)}'"""
              case t @ _ => throw new AssertionError(s"""'$t' isn't a supported type for the '-v@type' option""")
            })
            i += 2
          case "-b" =>
            assume(arg.size> i + 1, "'-b' option requires a argument")
            before.append(arg(i+1))
            i += 2
          case "-e" =>
            assume(arg.size> i + 1, "'-e' option requires a argument")
            after.append(arg(i+1))
            i += 2
          case "--" =>
            i += 1
            break
          case str =>
            process = Some(str)
            i += 1
            break
        }
      }
    }
    CliOption(arg.drop(i), before.toList, process, after.toList)
  }
}
