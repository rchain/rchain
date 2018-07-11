import Dependencies._
import BNFC._
import Rholang._
import NativePackagerHelper._
import com.typesafe.sbt.packager.docker._

lazy val projectSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.4",
  version := "0.1.0-SNAPSHOT",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")),
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

lazy val shared = (project in file("shared"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ Seq(
      catsCore,
      catsMtl,
      monix,
      scodecCore,
      scodecBits
    )
  )

lazy val casper = (project in file("casper"))
  .settings(commonSettings: _*)
  .settings(rholangSettings: _*)
  .settings(
    name := "casper",
    libraryDependencies ++= commonDependencies ++ protobufLibDependencies ++ Seq(
      catsCore,
      catsMtl,
      monix
    ),
    rholangProtoBuildAssembly := (rholangProtoBuild/Compile/incrementalAssembly).value
  )
  .dependsOn(comm % "compile->compile;test->test", shared, crypto, models, rspace, rholang, rholangProtoBuild)

lazy val comm = (project in file("comm"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
      grpcNetty,
      scalapbRuntimegGrpc,
      scalaUri,
      weupnp,
      hasher,
      catsCore,
      catsMtl,
      monix,
      guava
    ),
    PB.targets in Compile := Seq(
      PB.gens.java                        -> (sourceManaged in Compile).value,
      scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
    )
  ).dependsOn(shared, crypto)

lazy val crypto = (project in file("crypto"))
  .settings(commonSettings: _*)
  .settings(
    name := "crypto",
    libraryDependencies ++= commonDependencies ++ protobufLibDependencies ++ Seq(
      guava,
      bouncyCastle,
      scalacheckNoTest,
      kalium,
      jaxb,
      secp256k1Java,
      scodecBits),
    fork := true,
    doctestTestFramework := DoctestTestFramework.ScalaTest
  )

lazy val models = (project in file("models"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      catsCore,
      scalacheck,
      scalacheckShapeless,
      scalapbRuntimegGrpc
    ),
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
    )
  )
  .dependsOn(rspace)

lazy val node = (project in file("node"))
  .settings(commonSettings: _*)
  .enablePlugins(RpmPlugin, DebianPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(
    version := "0.4.2",
    name := "rnode",
    maintainer := "Pyrofex, Inc. <info@pyrofex.net>",
    packageSummary := "RChain Node",
    packageDescription := "RChain Node - the RChain blockchain node server software.",
    libraryDependencies ++=
      apiServerDependencies ++ commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
        catsCore,
        grpcNetty,
        nettyBoringSsl,
        jline,
        scallop,
        scalaUri,
        scalapbRuntimegGrpc
      ),
    PB.targets in Compile := Seq(
      PB.gens.java                        -> (sourceManaged in Compile).value / "protobuf",
      scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value / "protobuf"
    ),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, git.gitHeadCommit),
    buildInfoPackage := "coop.rchain.node",
    mainClass in assembly := Some("coop.rchain.node.Main"),
    assemblyMergeStrategy in assembly := {
      case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    },
    /* Dockerization */
    dockerUsername := Some(organization.value),
    dockerUpdateLatest := true,
    dockerBaseImage := "openjdk:8u171-jre-slim-stretch",
    dockerCommands := {
      val daemon = (daemonUser in Docker).value
      Seq(
        Cmd("FROM", dockerBaseImage.value),
        ExecCmd("RUN", "apt", "update"),
        ExecCmd("RUN", "apt", "install", "-yq", "libsodium18"),
        ExecCmd("RUN", "apt", "install", "-yq", "openssl"),
        Cmd("LABEL", s"""MAINTAINER="${maintainer.value}""""),
        Cmd("WORKDIR", (defaultLinuxInstallLocation in Docker).value),
        Cmd("ADD", s"--chown=$daemon:$daemon opt /opt"),
        Cmd("USER", daemon),
        ExecCmd("ENTRYPOINT", "bin/rnode", "--profile=docker"),
        ExecCmd("CMD", "run")
      )
    },
    mappings in Docker ++= {
       val base = (defaultLinuxInstallLocation in Docker).value
       directory((baseDirectory in rholang).value / "examples")
         .map { case (f, p) => f -> s"$base/$p" }
     },
    /* Packaging */
    mappings in packageZipTarball in Universal += baseDirectory.value / "macos_install.sh" -> "macos_install.sh",
    linuxPackageMappings ++= {
      val file = baseDirectory.value / "rnode.service"
      val rholangExamples = directory((baseDirectory in rholang).value / "examples")
        .map { case (f, p) => (f, s"/usr/share/rnode/$p") }
      Seq(packageMapping(file -> "/lib/systemd/system/rnode.service"), packageMapping(rholangExamples:_*))
    },
    /* Debian */
    debianPackageDependencies in Debian ++= Seq("openjdk-8-jre-headless (>= 1.8.0.171)",
                                                "openssl(>= 1.0.2g) | openssl(>= 1.1.0f)",  //ubuntu & debian
                                                "bash (>= 2.05a-11)",
                                                "libsodium18 (>= 1.0.8-5) | libsodium23 (>= 1.0.16-2)"),
    /* Redhat */
    rpmVendor := "rchain.coop",
    rpmUrl := Some("https://rchain.coop"),
    rpmLicense := Some("Apache 2.0"),
    packageArchitecture in Rpm := "noarch",
    maintainerScripts in Rpm := maintainerScriptsAppendFromFile((maintainerScripts in Rpm).value)(
      RpmConstants.Post -> (sourceDirectory.value / "rpm" / "scriptlets" / "post")
    ),
    rpmPrerequisites := Seq("java-1.8.0-openjdk-headless >= 1.8.0.171",
                        //"openssl >= 1.0.2k | openssl >= 1.1.0h", //centos & fedora but requires rpm 4.13 for boolean
                        "openssl",
                        "libsodium >= 1.0.14-1")
  )
  .dependsOn(casper, comm, crypto, rholang)

lazy val regex = (project in file("regex"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val rholang = (project in file("rholang"))
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)
  .settings(
    name := "rholang",
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Yno-adapted-args"
    ),
    libraryDependencies ++= commonDependencies ++ Seq(catsMtl, monix, scallop),
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
  .dependsOn(models % "compile->compile;test->test", rspace  % "compile->compile;test->test", crypto)

lazy val rholangCLI = (project in file("rholang-cli"))
  .settings(commonSettings: _*)
  .settings(
    mainClass in assembly := Some("coop.rchain.rholang.interpreter.RholangCLI")
  )
  .dependsOn(rholang)

lazy val rholangProtoBuildJar = Def.task(
  (assemblyOutputPath in (assembly)).value
)
lazy val _incrementalAssembly = Def.taskDyn(
  if (jarOutDated((rholangProtoBuildJar).value, (Compile / scalaSource).value))
    (assembly)
  else 
    rholangProtoBuildJar
)
lazy val incrementalAssembly = taskKey[File]("Only assemble if sources are newer than jar")
lazy val rholangProtoBuild = (project in file("rholang-proto-build"))
  .settings(commonSettings: _*)
  .settings(
    name := "rholang-proto-build",
    incrementalAssembly in Compile := _incrementalAssembly.value
  )
  .dependsOn(rholang)

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
    libraryDependencies ++= commonDependencies
  ).dependsOn(roscala_macros)

lazy val rspace = (project in file("rspace"))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin, TutPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "rspace",
    version := "0.2.1-SNAPSHOT",
    libraryDependencies ++= commonDependencies ++ Seq(
      lmdbjava,
      catsCore,
      scodecCore,
      scodecCats,
      scodecBits,
      guava
    ),
    /* Tutorial */
    tutTargetDirectory := (baseDirectory in Compile).value / ".." / "docs" / "rspace",
    /* Publishing Settings */
    scmInfo := Some(ScmInfo(url("https://github.com/rchain/rchain"), "git@github.com:rchain/rchain.git")),
    git.remoteRepo := scmInfo.value.get.connection,
    useGpg := true,
    pomIncludeRepository := { _ => false },
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://www.rchain.coop")),
    developers := List(
      Developer(
        id    = "guardbotmk3",
        name  = "Kyle Butt",
        email = "kyle@pyrofex.net",
        url   = url("https://www.pyrofex.net")
      ),
      Developer(
        id    = "ys-pyrofex",
        name  = "Yaraslau Levashkevich",
        email = "yaraslau@pyrofex.net",
        url   = url("https://www.pyrofex.net")
      ),
      Developer(
        id    = "KentShikama",
        name  = "Kent Shikama",
        email = "kent@kentshikama.com",
        url   = url("https://www.rchain.coop")
      ),
      Developer(
        id    = "henrytill",
        name  = "Henry Till",
        email = "henrytill@gmail.com",
        url   = url("https://www.pyrofex.net")
      )
    )
  )
  .dependsOn(shared, crypto)

lazy val rspaceBench = (project in file("rspace-bench"))
  .settings(commonSettings, libraryDependencies ++= commonDependencies)
  .enablePlugins(JmhPlugin)
  .dependsOn(rspace)

lazy val rchain = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(
    casper,
    comm,
    crypto,
    models,
    node,
    regex,
    rholang,
    rholangCLI,
    roscala,
    rspace,
    rspaceBench,
    shared
  )
