package wok.core

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scalax.file.Path


case class CliOption(args: List[String], script: String, diag: Boolean)


object CliOption {

  def parse(args: List[String]) = {
    var i = 0
    val variables, script = ListBuffer[String]()
    var diag = false

    breakable {
      while(true) {
        if (args.size == i) break
        args(i) match {
          case "-v" =>
            assume(args.size > i+1, "'-v' option requires an argument")
            val a1 = args(i+1)
            val idx = a1.indexOf("=")
            assume(idx != -1)
            assume(idx != a1.size - 1)
            variables.append(s"var ${a1.take(idx)} = ${a1.drop(idx+1)}")
            i += 2
          case a0 if a0.startsWith("-v@") =>
            assume(args.size > i+1, s"'-v@type' option requires an argument")
            val a1 = args(i+1)
            val idx = a1.indexOf("=")
            assume(idx != -1)
            assume(idx != a1.size-1)
            variables.append(a0.drop(3) match {
              case "str"    => s"""var ${a1.take(idx)} = \"${a1.drop(idx+1)}\""""
              case "rawstr" => s"""var ${a1.take(idx)} = \"\"\"${a1.drop(idx+1)}\"\"\""""
              case "char"   => s"""var ${a1.take(idx)} = '${a1.drop(idx+1)}'"""
              case t @ _ => throw new AssertionError(s"'$t' isn't a supported type for the '-v@type' option")
            })
            i += 2
          case "--diag" =>
            diag = true
            i += 1
          case "-f" =>
            assume(args.size > i+1, "'-f' option requires an argument")
            val f = Path.fromString(args(i+1))
            assume(f.exists, s"'$f' doesn't exist")
            script.append(f.string match {
              case s if s.startsWith("#") => "//" + s
              case s => s
            })
            i += 2
          case "--" =>
            i += 1
            break
          case str =>
            if (script.isEmpty) {
              script.append(str)
              i += 1
            }
            break
        }
      }
    }
    CliOption(args.drop(i), variables ++ script mkString "\n", diag)
  }
}
