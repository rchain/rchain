scalaVersion := "2.12.4"
organization := "coop.rchain"
version      := "0.0.1"

PB.targets in Compile := Seq(
  PB.gens.java -> (sourceManaged in Compile).value,
  scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
)

mainClass in assembly := Some("coop.rchain.comm.Main")

libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  // URI Parsing
  "io.lemonlabs" %% "scala-uri" % "0.5.0",

  // Command-line parsing
  "org.rogach" %% "scallop" % "3.0.3",

  // Hashing
  "org.scorexfoundation" %% "scrypto" % "2.0.0",

  // uPNP library
  "org.bitlet" % "weupnp" % "0.1.+",

  // Logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)



addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
)

lazy val commonOptions = Seq(
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfuture",
  "-Xlint:_,-unused",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked")

scalacOptions ++= commonOptions

logBuffered in Test := false
