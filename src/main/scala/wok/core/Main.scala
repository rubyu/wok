package wok.core

import wok.reflect.DynamicCompiler


object Main {
  def main(args: Array[String]) {
    val cli = CliOption.parse(args.toList)
    DynamicCompiler
      .compile(cli.before, cli.process, cli.after)
      .create(cli.arg)
      .runScript()
  }
}
