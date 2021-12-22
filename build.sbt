
scalaVersion := "2.13.7"

name := "aoc2021"
organization := "jostrander"
version := "1.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
)
