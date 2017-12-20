scalaVersion := "2.12.4"
organization := "rchain"
version      := "0.1"

PB.targets in Compile := Seq(
  PB.gens.java -> (sourceManaged in Compile).value,
  scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
)

mainClass in assembly := Some("coop.rchain.comm.Main")

libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  // URI Parsing
  "io.lemonlabs" %% "scala-uri" % "0.5.0",

  // Command-line parsing
  "org.rogach" %% "scallop" % "3.0.3",

  // Hashing
  "org.scorexfoundation" %% "scrypto" % "2.0.0",

  // uPNP library
  "org.bitlet" % "weupnp" % "0.1.+",

  // Logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
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

scalacOptions ++= commonOptions

logBuffered in Test := false

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
