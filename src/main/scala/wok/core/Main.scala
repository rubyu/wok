package wok.core

import wok.reflect.{AbstractWok, DynamicCompiler}


object Main {
  def main(args: Array[String]) {
    val cli = CliOption.parse(args.toList)
    val Wok = DynamicCompiler.compile(cli.before, cli.process, cli.after)
    val wok: AbstractWok =
      try Wok.create(cli.arg)
      catch {
        case e: ClassNotFoundException => System.exit(1); throw e
      }
    wok.runScript()
  }
}
