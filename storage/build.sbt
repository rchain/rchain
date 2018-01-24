lazy val rootSettingsDependencies = Seq(
  libraryDependencies ++= Seq(
    "org.lmdbjava"   % "lmdbjava"  % "0.0.2",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  )
)

lazy val rootSettings = Seq(
  organization := "coop.rchain",
  name := "storage",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.4",
  connectInput in run := true,
  logBuffered in Test := false,
)

lazy val root = (project in file("."))
  .settings(rootSettings: _*)
  .settings(rootSettingsDependencies: _*)
