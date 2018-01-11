import Dependencies._

lazy val macros = project

lazy val root = (project in file("."))
  .settings(
    name := "Rosette",
    mainClass in assembly := Some("coop.rchain.rosette.Main"),
    assemblyJarName in assembly := "rosette.jar",
    inThisBuild(List(
      organization := "coop.rchain",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT",
      scalafmtOnCompile in Compile := true,
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
    )),
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.chuusai" %% "shapeless" % "2.3.2",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "org.typelevel" %% "cats" % "0.9.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      scalaTest % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
    )
  ).dependsOn(macros)

