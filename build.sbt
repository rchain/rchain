import Dependencies._
import BNFC._
import NativePackagerHelper._

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

lazy val shared = (project in file("shared"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ Seq(
      catsCore,
      monix,
      scodecCore,
      scodecBits
    )
  )

lazy val casper = (project in file("casper"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      catsCore,
      catsMtl,
      monix
    )
  )
  .dependsOn(comm % "compile->compile;test->test", shared, crypto, models, rspace, rholang)

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
      monix,
      guava
    ),
    PB.targets in Compile := Seq(
      PB.gens.java                        -> (sourceManaged in Compile).value,
      scalapb.gen(javaConversions = true) -> (sourceManaged in Compile).value
    )
  ).dependsOn(shared)

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
  .enablePlugins(sbtdocker.DockerPlugin, RpmPlugin, DebianPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(
    version := "0.3.1",
    name := "rnode",
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
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "coop.rchain.node",
    mainClass in assembly := Some("coop.rchain.node.Main"),
    assemblyMergeStrategy in assembly := {
      case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    },
    /* Dockerization */
    dockerfile in docker := {
      val artifact: File     = assembly.value
      val artifactTargetPath = s"/${artifact.name}"
      val entry: File        = baseDirectory(_ / "main.sh").value
      val entryTargetPath    = "/bin"
      val rholangExamples = (baseDirectory in rholang).value / "examples"
      new Dockerfile {
        from("openjdk:8u171-jre-slim-stretch")
        add(artifact, artifactTargetPath)
        copy(rholangExamples, "/usr/share/rnode/examples")
        env("RCHAIN_TARGET_JAR", artifactTargetPath)
        add(entry, entryTargetPath)
        run("apt", "update")
        run("apt", "install", "-yq", "libsodium18")
        run("apt", "install", "-yq", "openssl")
        entryPoint("/bin/main.sh")
      }
    },
    /* Packaging */
    maintainer in Linux := "Pyrofex, Inc. <info@pyrofex.net>",
    packageSummary in Linux := "RChain Node",
    packageDescription in Linux := "RChain Node - the RChain blockchain node server software.",
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
  .dependsOn(models % "compile->compile;test->test", rspace  % "compile->compile;test->test", crypto)

lazy val rholangCLI = (project in file("rholang-cli"))
  .settings(commonSettings: _*)
  .settings(
    mainClass in assembly := Some("coop.rchain.rholang.interpreter.RholangCLI")
  )
  .dependsOn(rholang)

lazy val roscala = (project in file("roscala"))
  .settings(commonSettings: _*)
  .settings(
    name := "Rosette",
    mainClass in assembly := Some("coop.rchain.rosette.Main"),
    assemblyJarName in assembly := "rosette.jar",
    inThisBuild(
      List(addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))),
    libraryDependencies ++= commonDependencies
  )

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
      scodecBits
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
  .aggregate(casper, crypto, comm, models, regex, rspace, node, rholang, rholangCLI, roscala)
