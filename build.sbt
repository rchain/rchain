lazy val commonSettings = Seq(
  organization := "coop.rchain",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.4"
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
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked"
)

lazy val root = (project in file("."))
  .aggregate(node, comm)

lazy val comm = project
  .settings(
    commonSettings,
    scalacOptions ++= commonOptions
  )

lazy val node = project
  .settings(
    commonSettings,
    scalacOptions ++= commonOptions
  )
  .dependsOn(comm)

/*
 * Dockerization via sbt-docker
 */
enablePlugins(DockerPlugin)

dockerfile in docker := {
  val artifact: File = assembly.value
  val artifactTargetPath = s"/${artifact.name}"
  val entry: File = baseDirectory(_ / "main.sh").value
  val entryTargetPath = "/bin"
  new Dockerfile {
    from("openjdk:8u151-jre-alpine")
    add(artifact, artifactTargetPath)
    env("RCHAIN_TARGET_JAR", artifactTargetPath)
    add(entry, entryTargetPath)
    entryPoint("/bin/main.sh")
  }
}

imageNames in docker := Seq(
  ImageName(s"${organization.value}/${organization.value}-${name.value}:latest"),
  ImageName(s"${organization.value}/${organization.value}-${name.value}:v${version.value}")
)
