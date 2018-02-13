lazy val commonSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value),
  exportJars := true
)

lazy val commonSettingsDependencies = Seq(
  libraryDependencies ++= Seq(
    // format: off
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
    // format: on
  )
)

lazy val storageSettings = Seq(
  name := "storage",
  version := "0.1.0-SNAPSHOT",
  PB.targets in Compile := Seq(scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value)
)

lazy val storageSettingsDependencies = Seq(
  libraryDependencies ++= Seq(
    // format: off
    "org.lmdbjava"   % "lmdbjava"  % "0.6.0",
    "org.typelevel" %% "cats-core" % "1.0.1"
    // format: on
  )
)

lazy val coverageSettings = Seq(
  coverageMinimum := 90,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := Seq(
    (sourceManaged in Compile).value.getPath ++ "/.*"
  ).mkString(";")
)

lazy val storage = (project in file("."))
  .settings(CompilerSettings.options: _*)
  .settings(commonSettings: _*)
  .settings(commonSettingsDependencies: _*)
  .settings(storageSettings: _*)
  .settings(storageSettingsDependencies: _*)
  .settings(coverageSettings: _*)
