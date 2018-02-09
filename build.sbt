import Dependencies._
import BNFC._

def commonSettings: Seq[Setting[_]] =
  Seq[SettingsDefinition](
    
    organization := "coop.rchain",
    scalaVersion := "2.12.4",

    version := "0.1.0-SNAPSHOT",
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq(
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
    ),
    logBuffered in Test := false,
    crossScalaVersions := Seq("2.10.6", scalaVersion.value),
  ).flatMap(_.settings)

lazy val root = (project in file("."))
  .aggregate(node, comm)

lazy val comm = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      uriParsing,
      uPnP),
    PB.targets in Compile := Seq(
      PB.gens.java -> (sourceManaged in Compile).value,
      scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
    ),
  )

lazy val storage = project
  .settings(
    commonSettings,
    libraryDependencies += lmdb,
    connectInput in run := true,
  )

lazy val node = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ protobufDependencies,
    libraryDependencies ++= Seq(
      argParsing,
      uriParsing
    ),
    mainClass in assembly := Some("coop.rchain.node.Main")
  )
  .dependsOn(comm)

lazy val rholang = project
  .settings(
    commonSettings,
    crossScalaVersions := Seq("2.10.6", scalaVersion.value),
    bnfcSettings,
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
    mainClass in assembly := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler")
  )


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
