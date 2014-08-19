package wok.core

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scalax.file.Path


case class CliOption(arg: List[String], before: List[String], process: List[String], after: List[String], diag: Boolean)


object CliOption {

  def parse(arg: List[String]) = {
    var i = 0
    val before, process, after = ListBuffer[String]()
    var diag = false

    breakable {
      while(true) {
        if (arg.size == i) break
        arg(i) match {
          case "-v" =>
            assume(arg.size > i+1, "'-v' option requires an argument")
            val a1 = arg(i+1)
            val idx = a1.indexOf("=")
            assume(idx != -1)
            assume(idx != a1.size - 1)
            before.append(s"var ${a1.take(idx)} = ${a1.drop(idx+1)}")
            i += 2
          case a0 if a0.startsWith("-v@") =>
            assume(arg.size > i+1, s"'-v@type' option requires an argument")
            val a1 = arg(i+1)
            val idx = a1.indexOf("=")
            assume(idx != -1)
            assume(idx != a1.size-1)
            before.append(a0.drop(3) match {
              case "str"    => s"""var ${a1.take(idx)} = \"${a1.drop(idx+1)}\""""
              case "rawstr" => s"""var ${a1.take(idx)} = \"\"\"${a1.drop(idx+1)}\"\"\""""
              case "char"   => s"""var ${a1.take(idx)} = '${a1.drop(idx+1)}'"""
              case t @ _ => throw new AssertionError(s"'$t' isn't a supported type for the '-v@type' option")
            })
            i += 2
          case "-b" =>
            assume(arg.size > i+1, "'-b' option requires an argument")
            before.appendAll(arg(i+1).split("""(\r\n|\r|\n)"""))
            i += 2
          case "-e" =>
            assume(arg.size > i+1, "'-e' option requires an argument")
            after.appendAll(arg(i+1).split("""(\r\n|\r|\n)"""))
            i += 2
          case "--diag" =>
            diag = true
            i += 1
          case "-f" =>
            assume(arg.size > i+1, "'-f' option requires an argument")
            val f = Path.fromString(arg(i+1))
            assume(f.exists, s"'$f' doesn't exist")
            f.lines().groupBy {
              case s if s.startsWith("-b ") => 0
              case s if s.startsWith("-e ") => 1
              case _ => 2
            } foreach {
              case (0, xs) => xs foreach { s => before.append(s drop 3) }
              case (1, xs) => xs foreach { s => after.append(s drop 3) }
              case (2, xs) => process.appendAll(xs)
            }
            i += 2
            break
          case "--" =>
            i += 1
            break
          case str =>
            process.appendAll(str.split("""(\r\n|\r|\n)"""))
            i += 1
            break
        }
      }
    }
    CliOption(arg.drop(i), before.toList, process.toList, after.toList, diag)
  }
}
