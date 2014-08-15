package wok.core

import wok.reflect.DynamicCompiler


object Main {
  def main(args: Array[String]) {
    val cli = CliOption.parse(args.toList)
    val (report, classWok) = DynamicCompiler.compile(cli.before, cli.process, cli.after)
    if (cli.diag) {
      Console.out.println("The results of diagnosis are:")
      Console.out.println(report)
    }
    else if (report.hasError) {
      Console.err.println("Compilation failed. The details are:")
      Console.err.println(report)
      System.exit(1)
    }
    else {
      classWok
        .create(cli.arg)
        .runScript()
    }
  }
}
