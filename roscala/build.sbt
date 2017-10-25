import Dependencies._

lazy val root = (project in file("."))
  .settings(
    mainClass in assembly := Some("coop.rchain.rosette.Main"),
    inThisBuild(List(
      assemblyJarName in assembly := "rosette.jar",
      organization := "coop.rchain",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT",
      scalafmtOnCompile in Compile := true
    )),
    name := "Rosette",
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.chuusai" %% "shapeless" % "2.3.2",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "org.typelevel" %% "cats" % "0.9.0",
      scalaTest % Test
    )
  )
