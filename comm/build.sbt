import sbt.Keys._

organization := "rchain"
//scalaVersion := "2.11.8"
version      := "0.0.1"
name := "RChain Communication"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.5.0",
  "org.json4s" %% "json4s-jackson" % "3.5.0",
  "io.jvm.uuid" %% "scala-uuid" % "0.2.2",
  "org.abstractj.kalium" % "kalium" % "0.6.0"
)

logBuffered in Test := false
