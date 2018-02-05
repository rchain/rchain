lazy val compilerOptions = Seq(
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint:-unused",
  "-Ypartial-unification",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked"
)

lazy val nonConsoleOptions = Set(
  "-Ywarn-unused-import",
  "-Xfatal-warnings"
)

lazy val coverageSettings = Seq(
  coverageMinimum := 90,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := Seq(
    (sourceManaged in Compile).value.getPath ++ "/.*"
  ).mkString(";")
)

lazy val commonSettingsDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  )
)

lazy val commonSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.4",
  scalacOptions ++= compilerOptions,
  scalacOptions in (Compile, console) ~= { _.filterNot(nonConsoleOptions) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val storageSettingsDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.lmdbjava"   % "lmdbjava"  % "0.6.0",
    "org.typelevel" %% "cats-core" % "1.0.1"
  )
)

lazy val storageSettings = Seq(
  name := "storage",
  version := "0.1.0-SNAPSHOT",
  PB.targets in Compile := Seq(scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value)
)

lazy val storage = (project in file("."))
  .settings(commonSettings: _*)
  .settings(commonSettingsDependencies: _*)
  .settings(storageSettings: _*)
  .settings(storageSettingsDependencies: _*)
  .settings(coverageSettings: _*)
