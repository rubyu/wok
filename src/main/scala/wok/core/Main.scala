package wok.core

import wok.reflect.DynamicCompiler
import wok.reflect.Helpers.ExtendedInputStreamResource


object Main {
  def main(args: Array[String]) {
    val cli = CliOption.parse(args.toList)
    val (report, classWok) = DynamicCompiler.compile(cli.script)
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
      // Releases memory used for compilation immediately.
      System.gc()

      classWok
        .create(cli.args)
        .runScript()
    }
  }
}
