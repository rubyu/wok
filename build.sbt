
import AssemblyKeys._

organization := "com.github.rubyu"

name := "wok"

version := "0.0.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.10.4",
    "com.github.scala-incubator.io" % "scala-io-core_2.10" % "0.4.3",
    "com.github.scala-incubator.io" % "scala-io-file_2.10" % "0.4.3",
    "org.specs2" % "specs2_2.10" % "2.3.12" % "test"
  )

mainClass in assembly := Some("wok.core.Main")

jarName in assembly <<= (name, version) { (name, version) => name + "-" + version + ".jar" }

assemblySettings

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "rootdoc.txt" => MergeStrategy.concat
    case x => old(x)
  }
}

test in assembly := {}

