import BNFC._

// needed for SuperSafe sbt plugin
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

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
  "-unchecked"
)

lazy val consoleOptions = commonOptions diff Seq("-Xlint", "-Ywarn-unused-import")

lazy val coverageSettings = Seq(
  coverageExcludedFiles := Seq(
    (javaSource in Compile).value,
    (bnfcGrammarDir in BNFCConfig).value,
    (bnfcOutputDir in BNFCConfig).value,
    baseDirectory.value / "src" / "main" / "k",
    baseDirectory.value / "src" / "main" / "rbl"
  ).map(_.getPath ++ "/.*").mkString(";")
)

lazy val commonSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.4",
  scalacOptions := commonOptions,
  scalacOptions in (Compile, console) := consoleOptions,
  scalacOptions in (Test, console) := consoleOptions
)

lazy val commonSettingsDependencies = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.4"),
    "org.scalactic" %% "scalactic"   % "3.0.4",
    "org.scalaz"    %% "scalaz-core" % "7.3.0-M17",
    "org.scalatest" %% "scalatest"   % "3.0.4" % "test"
  )
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(commonSettingsDependencies: _*)
  .settings(coverageSettings: _*)
  .settings(bnfcSettings: _*)
  .settings(
    name := "rholang",
    mainClass in (Compile, packageBin) := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler")
  )

