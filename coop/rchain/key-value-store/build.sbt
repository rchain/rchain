lazy val root = (project in file("."))
  .settings(
    name := "KeyValueStore",
    scalaVersion := "2.12.2"
  )

connectInput in run := true

