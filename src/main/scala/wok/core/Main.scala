package wok.core

import wok.reflect.{AbstractWok, DynamicCompiler}


object Main {
  def main(args: Array[String]) {
    val cli = CliOption.parse(args.toList)
    DynamicCompiler.compile(cli.before, cli.process, cli.after) match {
      case Left(report) =>
        Console.err.println("Compilation failed. The details are:")
        Console.err.println(report)
      case Right(classWok) =>
        classWok
          .create(cli.arg)
          .runScript()
    }
  }
}
