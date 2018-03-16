import Dependencies._
import BNFC._

def commonSettings: Seq[Setting[_]] =
  Seq[SettingsDefinition](

    organization := "coop.rchain",
    scalaVersion := "2.12.4",

    version := "0.1.0-SNAPSHOT",

    resolvers += Resolver.sonatypeRepo("releases"),

    CompilerSettings.options,
    logBuffered in Test := false,
    crossScalaVersions := Seq("2.10.6", scalaVersion.value),

    coverageMinimum := 90,
    coverageFailOnMinimum := false,
    coverageExcludedFiles := Seq(
      (sourceManaged in Compile).value.getPath ++ "/.*"
    ).mkString(";"),

    scalafmtOnCompile := true,

    /*
     * By default, tag docker images with organization and the
     * version.
     */
    imageNames in docker := Seq(
      ImageName(s"${organization.value}/${organization.value}-${name.value}:latest"),
      ImageName(s"${organization.value}/${organization.value}-${name.value}:v${version.value}")
    ),

    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

  ).flatMap(_.settings)

lazy val crypto = project
  .settings(
    name := "Crypto",
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      guava,
      bouncyCastle,
      kalium,
      jaxb),
    fork := true,
    unmanagedSourceDirectories in Compile += baseDirectory.value / "secp256k1/src/java",
    javaOptions += "-Djava.library.path=secp256k1/.libs",
    doctestTestFramework := DoctestTestFramework.ScalaTest
  )

lazy val comm = project
  .settings(
    commonSettings,
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
      uriParsing,
      uPnP,
      hasher,
      cats,
      monix,
      guava
    ),
    PB.targets in Compile := Seq(
      PB.gens.java -> (sourceManaged in Compile).value,
      scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
    ),
    coverageExcludedFiles := Seq(
      (javaSource in Compile).value,
      (sourceManaged in Compile).value
    ).map(_.getPath ++ "/.*").mkString(";")
  )

lazy val models = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      cats,
      scalaCheck,
      scalaCheckShapeless
    ),
    connectInput in run := true,
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
    ),
    crossScalaVersions := Seq("2.11.12", scalaVersion.value),
    exportJars := true
  )

lazy val regex = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ Seq(
      scalaCheck
    ),
    connectInput in run := true,
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
    ),
    crossScalaVersions := Seq("2.11.12", scalaVersion.value),
    exportJars := true
  )

lazy val storage = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      lmdb,
      cats
    ),
    connectInput in run := true,
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
    ),
    crossScalaVersions := Seq("2.11.12", scalaVersion.value),
    exportJars := true
  ).dependsOn(models)

lazy val node = project
  .enablePlugins(sbtdocker.DockerPlugin, RpmPlugin, DebianPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(
    commonSettings,
    version := "0.1.3",
    name := "rnode",
    libraryDependencies ++= commonDependencies ++ protobufDependencies,
    libraryDependencies ++= Seq(
      argParsing,
      uriParsing
    ),
    libraryDependencies ++= apiServerDependencies ++ kamonDependencies ++ Seq(cats),

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "coop.rchain.node",

    mainClass in assembly := Some("coop.rchain.node.Main"),

    /*
     * Dockerization via sbt-docker
     */

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
    },

    maintainer in Linux := "Pyrofex, Inc. <info@pyrofex.net>",
    packageSummary in Linux := "RChain Node",
    packageDescription in Linux := "RChain Node - the RChain blockchain node server software.",

    /*
     * Debian.
     */
    debianPackageDependencies in Debian ++= Seq("openjdk-8-jre-headless", "bash (>= 2.05a-11)"),

    /*
     * Redhat
     */
    rpmVendor := "rchain.coop",
    rpmUrl := Some("https://rchain.coop"),
    rpmLicense := Some("Apache 2.0")
  )
  .dependsOn(comm, crypto)

lazy val rholang = project
  .settings(
    commonSettings,
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Yno-adapted-args",
    ),
    libraryDependencies ++= commonDependencies ++ Seq(monix, argParsing),
    bnfcSettings,
    mainClass in assembly := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler"),
    coverageExcludedFiles := Seq(
      (javaSource in Compile).value,
      (bnfcGrammarDir in BNFCConfig).value,
      (bnfcOutputDir in BNFCConfig).value,
      baseDirectory.value / "src" / "main" / "k",
      baseDirectory.value / "src" / "main" / "rbl"
    ).map(_.getPath ++ "/.*").mkString(";"),

    // Fix up root directory so tests find relative files they need
    fork in Test := true
  ).dependsOn(models)

lazy val roscala_macros = (project in file("roscala/macros"))
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value)
  )

lazy val roscala = project
  .settings(
    commonSettings,
    name := "Rosette",
    mainClass in assembly := Some("coop.rchain.rosette.Main"),
    assemblyJarName in assembly := "rosette.jar",
    inThisBuild(List(
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))),
    libraryDependencies ++= commonDependencies ++ Seq(
      cats,
      shapeless,
      scalaCheck)
  ).dependsOn(roscala_macros)
