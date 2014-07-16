
import AssemblyKeys._

organization := "com.github.rubyu"

name := "wok"

version := "0.0.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
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

