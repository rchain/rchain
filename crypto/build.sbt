import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "coop.rchain",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Crypto",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scorexfoundation" %% "scrypto" % "2.0.0"
  )
