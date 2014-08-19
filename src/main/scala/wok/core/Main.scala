package wok.core

import wok.reflect.DynamicCompiler
import wok.reflect.Helpers.ExtendedInputStreamResource


object Main {
  def main(args: Array[String]) {
    val cli = CliOption.parse(args.toList)

    /** Blocks until the input data can be read for optimization of memory usage. For running a wok script Wok.scala
      * need to be compiled, but it was desirable that the compilation will be just-in-time because in the real
      * use case, some wok.jar can be cascaded.
      */
    if (!cli.diag && cli.process.nonEmpty) {
      while (Stdio.in.open().get.available() == 0) Thread.sleep(1)
    }

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
      // Releases memory used for compilation immediately.
      System.gc()

      classWok
        .create(cli.arg)
        .runScript()
    }
  }
}
