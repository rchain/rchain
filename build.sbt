organization := "coop.rchain"
scalaVersion := "2.11.8"
version      := "0.1.0-SNAPSHOT"
name := "RChain"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.mongodb.scala" %% "mongo-scala-driver" % "1.2.1",
  "org.json4s" %% "json4s-native" % "3.5.0",
  "org.json4s" %% "json4s-jackson" % "3.5.0",
  "io.jvm.uuid" %% "scala-uuid" % "0.2.2"
)
