
package wok.core

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.util.BatchSourceFile


class DynamicCompiler {
  private val virtualDirectory: VirtualDirectory = new VirtualDirectory("[memory]", None)
  private val settings: Settings = new Settings
  settings.deprecation.value = true
  settings.unchecked.value = true
  settings.outputDirs.setSingleOutput(virtualDirectory)
  settings.bootclasspath.value = System.getProperty("java.class.path")
  settings.classpath.value = settings.bootclasspath.value

  private val global = new Global(settings, new ConsoleReporter(settings))
  private val classLoader: AbstractFileClassLoader = new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)

  def compileClass(source: String) {
    val compiler = new global.Run
    compiler.compileSources(List(new BatchSourceFile("Wok.scala", source)))
  }

  def compile(arg: List[String], before: List[String], process: String, after: List[String]): AbstractWok =
    try {
      compileClass(wrapScript(before, process, after))
      classLoader.findClass("wok.Wok")
        .getConstructor(classOf[List[String]])
        .newInstance(arg)
        .asInstanceOf[AbstractWok]
    } finally virtualDirectory.clear

  def wrapScript(before: List[String], process: String, after: List[String]): String = {
    """
      |package wok
      |
      |import wok.core.AbstractWok
      |import wok.core.Helpers.{OpenableReader, OpenableInputStream, OpenablePath,
      |  PrintableOutputStream, PrintablePath, CompletableAny, CompletableIterator}
      |
      |class Wok(val arg: List[String]) extends AbstractWok {
      |%s
      |  {
      |    val result = Console.in.open() %s
      |    result.complete()
      |  }
      |%s
      |}
      |"""
      .stripMargin.format(before.map { "  " + _ }.mkString("\n"), process, after.map { "  " + _ }.mkString("\n"))
  }
}