import BNFC._

lazy val commonOptions = Seq(
  "-language:existentials",
  "-language:higherKinds",
//  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked")

lazy val consoleOptions = commonOptions diff Seq("-Xlint", "-Ywarn-unused-import")

lazy val commonSettings = Seq(
  name := "rholang",
  organization := "coop.rchain",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.10.6", scalaVersion.value),
  scalacOptions := commonOptions,
  scalacOptions in (Compile, console) := consoleOptions,
  scalacOptions in (Test, console) := consoleOptions)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)

// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M17"

// Scala Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")

// Kind projector
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

mainClass in (Compile, packageBin) := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler")
