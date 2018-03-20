import Dependencies._
import BNFC._

lazy val projectSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.4",
  version := "0.1.0-SNAPSHOT",
  resolvers += Resolver.sonatypeRepo("releases"),
  scalafmtOnCompile := true
)

lazy val coverageSettings = Seq(
  coverageMinimum := 90,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := Seq(
    (javaSource in Compile).value,
    (sourceManaged in Compile).value.getPath ++ "/.*"
  ).mkString(";")
)

lazy val compilerSettings = CompilerSettings.options ++ Seq(
  crossScalaVersions := Seq("2.11.12", scalaVersion.value)
)

lazy val commonSettings = projectSettings ++ coverageSettings ++ compilerSettings

lazy val comm = (project in file("comm"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
      scalaUri,
      weupnp,
      hasher,
      catsCore,
      monix,
      guava
    ),
    PB.targets in Compile := Seq(
      PB.gens.java                        -> (sourceManaged in Compile).value,
      scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
    )
  )

lazy val crypto = (project in file("crypto"))
  .settings(commonSettings: _*)
  .settings(
    name := "crypto",
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      guava,
      bouncyCastle,
      kalium,
      jaxb
    ),
    fork := true,
    unmanagedSourceDirectories in Compile += baseDirectory.value / "secp256k1/src/java",
    javaOptions += "-Djava.library.path=secp256k1/.libs",
    doctestTestFramework := DoctestTestFramework.ScalaTest
  )

lazy val models = (project in file("models"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      catsCore,
      scalacheck,
      scalacheckShapeless
    ),
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
    )
  )
  .dependsOn(storage)

lazy val node = (project in file("node"))
  .settings(commonSettings: _*)
  .enablePlugins(sbtdocker.DockerPlugin, RpmPlugin, DebianPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(
    version := "0.1.3",
    name := "rnode",
    libraryDependencies ++=
      apiServerDependencies ++ commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
        catsCore,
        scallop,
        scalaUri
      ),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "coop.rchain.node",
    mainClass in assembly := Some("coop.rchain.node.Main"),
    /* Dockerization */
    dockerfile in docker := {
      val artifact: File     = assembly.value
      val artifactTargetPath = s"/${artifact.name}"
      val entry: File        = baseDirectory(_ / "main.sh").value
      val entryTargetPath    = "/bin"
      new Dockerfile {
        from("openjdk:8u151-jre-alpine")
        add(artifact, artifactTargetPath)
        env("RCHAIN_TARGET_JAR", artifactTargetPath)
        add(entry, entryTargetPath)
        entryPoint("/bin/main.sh")
      }
    },
    /* Packaging */
    maintainer in Linux := "Pyrofex, Inc. <info@pyrofex.net>",
    packageSummary in Linux := "RChain Node",
    packageDescription in Linux := "RChain Node - the RChain blockchain node server software.",
    /* Debian */
    debianPackageDependencies in Debian ++= Seq("openjdk-8-jre-headless", "bash (>= 2.05a-11)"),
    /* Redhat */
    rpmVendor := "rchain.coop",
    rpmUrl := Some("https://rchain.coop"),
    rpmLicense := Some("Apache 2.0")
  )
  .dependsOn(comm, crypto)

lazy val regex = (project in file("regex"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val rholang = (project in file("rholang"))
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)
  .settings(
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Yno-adapted-args"
    ),
    libraryDependencies ++= commonDependencies ++ Seq(monix, scallop),
    mainClass in assembly := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler"),
    coverageExcludedFiles := Seq(
      (javaSource in Compile).value,
      (bnfcGrammarDir in BNFCConfig).value,
      (bnfcOutputDir in BNFCConfig).value,
      baseDirectory.value / "src" / "main" / "k",
      baseDirectory.value / "src" / "main" / "rbl"
    ).map(_.getPath ++ "/.*").mkString(";"),
    fork in Test := true
  )
  .dependsOn(models, storage)

lazy val roscala_macros = (project in file("roscala/macros"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val roscala = (project in file("roscala"))
  .settings(commonSettings: _*)
  .settings(
    name := "Rosette",
    mainClass in assembly := Some("coop.rchain.rosette.Main"),
    assemblyJarName in assembly := "rosette.jar",
    inThisBuild(
      List(addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))),
    libraryDependencies ++= commonDependencies ++ Seq(catsCore, shapeless, scalacheck)
  )
  .dependsOn(roscala_macros)

lazy val storage = (project in file("storage"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      lmdbjava,
      catsCore
    ),
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
    )
  )

lazy val rchain = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(crypto, comm, models, regex, storage, node, rholang, roscala)
