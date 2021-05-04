import Dependencies._
import BNFC._
import Rholang._
import NativePackagerHelper._
import com.typesafe.sbt.packager.docker._
//allow stopping sbt tasks using ctrl+c without killing sbt itself
Global / cancelable := true

//disallow any unresolved version conflicts at all for faster feedback
Global / conflictManager := ConflictManager.strict
//resolve all version conflicts explicitly
Global / dependencyOverrides := Dependencies.overrides

lazy val projectSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.11",
  version := "0.1.0-SNAPSHOT",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "jitpack" at "https://jitpack.io"
  ),
  wartremoverExcluded += sourceManaged.value,
  wartremoverErrors in (Compile, compile) ++= Warts.allBut(
    // those we want
    Wart.DefaultArguments,
    Wart.ImplicitParameter,
    Wart.ImplicitConversion,
    Wart.LeakingSealed,
    Wart.Recursion,
    // those don't want
    Wart.Overloading,
    Wart.Nothing,
    Wart.Equals,
    Wart.PublicInference,
    Wart.TraversableOps,
    Wart.ArrayEquals,
    Wart.While,
    Wart.Any,
    Wart.Product,
    Wart.Serializable,
    Wart.OptionPartial,
    Wart.EitherProjectionPartial,
    Wart.Option2Iterable,
    Wart.ToString,
    Wart.JavaConversions,
    Wart.MutableDataStructures,
    Wart.FinalVal,
    Wart.Null,
    Wart.AsInstanceOf,
    Wart.ExplicitImplicitTypes,
    Wart.StringPlusAny,
    Wart.AnyVal
  ),
  scalafmtOnCompile := sys.env.get("CI").isEmpty, // disable in CI environments
  scapegoatVersion in ThisBuild := "1.4.6",
  testOptions in Test += Tests.Argument("-oD"), //output test durations
  dependencyOverrides ++= Seq(
    "io.kamon" %% "kamon-core" % kamonVersion
  ),
  javacOptions ++= Seq("-source", "11", "-target", "11"),
  Test / fork := true,
  Test / parallelExecution := false,
  Test / testForkedParallel := false,
  IntegrationTest / fork := true,
  IntegrationTest / parallelExecution := false,
  IntegrationTest / testForkedParallel := false,
  assemblyMergeStrategy in assembly := {
    // For some reason, all artifacts from 'io.netty' group contain this file with different contents.
    // Discarding it as it's not needed.
    case path if path.endsWith("io.netty.versions.properties") => MergeStrategy.discard
    // The scala compiler includes native bindings for jansi under the same path jansi does.
    // This should pick the ones provided by jansi.
    case path if path.startsWith("META-INF/native/") && path.contains("jansi") => MergeStrategy.last
    case path                                                                  => MergeStrategy.defaultMergeStrategy(path)
  }
) ++
// skip api doc generation if SKIP_DOC env variable is defined
  Seq(sys.env.get("SKIP_DOC")).flatMap { _ =>
    Seq(
      publishArtifact in (Compile, packageDoc) := false,
      publishArtifact in packageDoc := false,
      sources in (Compile, doc) := Seq.empty
    )
  }

// a namespace for generative tests (or other tests that take a long time)
lazy val SlowcookerTest = config("slowcooker") extend (Test)

lazy val coverageSettings = Seq(
  coverageMinimum := 90,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := Seq(
    (javaSource in Compile).value,
    (sourceManaged in Compile).value.getPath ++ "/.*"
  ).mkString(";")
)

lazy val compilerSettings = CompilerSettings.options ++ Seq(
  crossScalaVersions := Seq(scalaVersion.value)
)

// Before starting sbt export YOURKIT_AGENT set to the profiling agent appropriate
// for your OS (https://www.yourkit.com/docs/java/help/agent.jsp)
lazy val profilerSettings = Seq(
  javaOptions in run ++= sys.env
    .get("YOURKIT_AGENT")
    .map(agent => s"-agentpath:$agent=onexit=snapshot,sampling")
    .toSeq,
  javaOptions in reStart ++= (javaOptions in run).value
)

lazy val commonSettings = projectSettings ++ coverageSettings ++ compilerSettings ++ profilerSettings

lazy val shared = (project in file("shared"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ Seq(
      catsCore,
      catsEffect,
      catsMtl,
      catsTagless,
      fs2Core,
      lz4,
      monix,
      scodecCore,
      scodecCats,
      scodecBits,
      scalapbRuntimegGrpc,
      lmdbjava,
      catsEffectLawsTest,
      catsLawsTest,
      catsLawsTestkitTest,
      enumeratum,
    )
  )

lazy val graphz = (project in file("graphz"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ Seq(
      catsCore,
      catsEffect,
      catsMtl
    )
  )
  .dependsOn(shared)

lazy val casper = (project in file("casper"))
  .configs(SlowcookerTest)
  .settings(commonSettings: _*)
  .settings(rholangSettings: _*)
  .settings(inConfig(SlowcookerTest)(Defaults.testSettings): _*)
  .settings(inConfig(SlowcookerTest)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings))
  .settings(
    name := "casper",
    libraryDependencies ++= commonDependencies ++ protobufLibDependencies ++ Seq(
      catsCore,
      catsRetry,
      catsMtl,
      monix,
      fs2Core,
      scalacheck % "slowcooker"
    )
  )
  .dependsOn(
    blockStorage % "compile->compile;test->test",
    comm         % "compile->compile;test->test",
    shared       % "compile->compile;test->test",
    graphz,
    crypto,
    models % "compile->compile;test->test",
    rspace,
    rholang % "compile->compile;test->test"
  )

lazy val comm = (project in file("comm"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    dependencyOverrides += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
      grpcNetty,
      nettyBoringSsl,
      scalapbRuntimegGrpc,
      scalaUri,
      weupnp,
      hasher,
      catsCore,
      catsMtl,
      catsTagless,
      monix,
      guava
    ),
    PB.targets in Compile := Seq(
      scalapb.gen(grpc = false)  -> (sourceManaged in Compile).value,
      grpcmonix.generators.gen() -> (sourceManaged in Compile).value
    )
  )
  .dependsOn(shared % "compile->compile;test->test", crypto, models)

lazy val crypto = (project in file("crypto"))
  .settings(commonSettings: _*)
  .settings(
    name := "crypto",
    libraryDependencies ++= commonDependencies ++ protobufLibDependencies ++ Seq(
      guava,
      bouncyPkixCastle,
      bouncyProvCastle,
      scalacheck,
      kalium,
      jaxb,
      secp256k1Java,
      scodecBits
    ),
    fork := true
  )
  .dependsOn(shared)

lazy val models = (project in file("models"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      catsCore,
      magnolia,
      scalapbCompiler,
      scalacheck % "test",
      scalacheckShapeless,
      scalapbRuntimegGrpc
    ),
    PB.targets in Compile := Seq(
      coop.rchain.scalapb.gen(flatPackage = true, grpc = false) -> (sourceManaged in Compile).value,
      grpcmonix.generators.gen()                                -> (sourceManaged in Compile).value
    )
  )
  .dependsOn(shared % "compile->compile;test->test", rspace)

lazy val node = (project in file("node"))
  .settings(commonSettings: _*)
  .enablePlugins(RpmPlugin, DebianPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(
    version := git.gitDescribedVersion.value.getOrElse({
      val v = "0.0.0-unknown"
      System.err.println("Could not get version from `git describe`.")
      System.err.println("Using the fallback version: " + v)
      v
    }),
    name := "rnode",
    maintainer := "RChain Cooperative https://www.rchain.coop/",
    packageSummary := "RChain Node",
    packageDescription := "RChain Node - the RChain blockchain node server software.",
    libraryDependencies ++=
      apiServerDependencies ++ commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
        catsCore,
        catsTagless,
        catsRetry,
        grpcNetty,
        grpcServices,
        jline,
        scallop,
        scalaUri,
        scalapbRuntimegGrpc,
        circeGenericExtras,
        pureconfig
      ),
    PB.targets in Compile := Seq(
      scalapb.gen(grpc = false)  -> (sourceManaged in Compile).value / "protobuf",
      grpcmonix.generators.gen() -> (sourceManaged in Compile).value / "protobuf"
    ),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, git.gitHeadCommit),
    buildInfoPackage := "coop.rchain.node",
    mainClass in Compile := Some("coop.rchain.node.Main"),
    mainClass in assembly := Some("coop.rchain.node.Main"),
    assemblyMergeStrategy in assembly := {
      case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    },
    /*
     * This monstrosity exists because
     * a) we want to get rid of annoying JVM >= 9 warnings,
     * b) we must support Java 8 for RedHat (see below) and
     * c) sbt-native-packager puts bashScriptExtraDefines before it
     *    initializes all useful variables (like $java_version).
     *
     * This won't work if someone passes -no-version-check command line
     * argument to rnode. They most probably know what they're doing.
     *
     * https://unix.stackexchange.com/a/29742/124070
     * Thanks Gilles!
     */
    bashScriptExtraDefines += """
      eval "original_$(declare -f java_version_check)"
      java_version_check() {
        original_java_version_check
        if [[ ${java_version%%.*} -ge 9 ]]; then
          java_args+=(
            --illegal-access=warn # set to deny if you feel brave
            --add-opens=java.base/java.nio=ALL-UNNAMED
            --add-opens=java.base/sun.nio.ch=ALL-UNNAMED
            --add-opens=java.base/sun.security.util=ALL-UNNAMED
            --add-opens=java.base/sun.security.x509=ALL-UNNAMED
            --add-opens=java.base/sun.security.provider=ALL-UNNAMED
          )
        fi
      }
    """,
    /* Dockerization */
    dockerUsername := Some(organization.value),
    dockerAliases ++=
      sys.env
        .get("DRONE_BUILD_NUMBER")
        .toSeq
        .map(num => dockerAlias.value.withTag(Some(s"DRONE-${num}"))),
    dockerUpdateLatest := sys.env.get("DRONE").isEmpty,
    dockerBaseImage := "openjdk:11-jre-slim",
    dockerCommands := {
      val daemon = (daemonUser in Docker).value
      Seq(
        Cmd("FROM", dockerBaseImage.value),
        ExecCmd("RUN", "apt", "update"),
        ExecCmd("RUN", "apt", "install", "-yq", "openssl", "openssh-server", "procps"),
        Cmd("LABEL", s"""MAINTAINER="${maintainer.value}""""),
        Cmd("WORKDIR", (defaultLinuxInstallLocation in Docker).value),
        Cmd("ADD", s"--chown=$daemon:$daemon opt /opt"),
        Cmd("USER", "root"),
        ExecCmd(
          "ENTRYPOINT",
          "bin/rnode",
          "--profile=docker",
          "-XX:ErrorFile=/var/lib/rnode/hs_err_pid%p.log"
        ),
        ExecCmd("CMD", "run")
      )
    },
    mappings in Docker ++= {
      val base = (defaultLinuxInstallLocation in Docker).value
      directory((baseDirectory in rholang).value / "examples")
        .map { case (f, p) => f -> s"$base/$p" }
    },
    /* Packaging */
    linuxPackageMappings ++= {
      val file = baseDirectory.value / "rnode.service"
      val rholangExamples = directory((baseDirectory in rholang).value / "examples")
        .map { case (f, p) => (f, s"/usr/share/rnode/$p") }
      Seq(
        packageMapping(file -> "/lib/systemd/system/rnode.service"),
        packageMapping(rholangExamples: _*)
      )
    },
    /* Debian */
    debianPackageDependencies in Debian ++= Seq(
      "openjdk-11-jre-headless",
      "openssl(>= 1.0.2g) | openssl(>= 1.1.1h)", //ubuntu & debian
      "bash (>= 2.05a-11)"
    ),
    /* Redhat */
    /*
     * RPM version string cannot contain dashes:
     *   http://ftp.rpm.org/max-rpm/ch-rpm-file-format.html
     */
    version in Rpm := version.value.replace('-', '.'),
    rpmVendor := "rchain.coop",
    rpmUrl := Some("https://rchain.coop"),
    rpmLicense := Some("Apache 2.0"),
    packageArchitecture in Rpm := "noarch",
    maintainerScripts in Rpm := maintainerScriptsAppendFromFile((maintainerScripts in Rpm).value)(
      RpmConstants.Post -> (sourceDirectory.value / "rpm" / "scriptlets" / "post")
    ),
    rpmPrerequisites := Seq(
      /*
       * https://access.redhat.com/articles/1299013
       * Red Hat will skip Java SE 9 and 10, and ship an OpenJDK distribution based on Java SE 11.
       */
      "java-1.8.0-openjdk-headless >= 1.8.0.171",
      //"openssl >= 1.0.2k | openssl >= 1.1.0h", //centos & fedora but requires rpm 4.13 for boolean
      "openssl"
    )
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
      "-Yno-adapted-args",
      "-Xfatal-warnings",
      "-Xlint:_,-missing-interpolator" // disable "possible missing interpolator" warning
    ),
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile, doc) := Seq.empty,
    libraryDependencies ++= commonDependencies ++ Seq(
      catsMtl,
      catsEffect,
      monix,
      scallop,
      lightningj,
      catsLawsTest,
      catsLawsTestkitTest,
      catsMtlLawsTest
    ),
    mainClass in assembly := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler"),
    coverageExcludedFiles := Seq(
      (javaSource in Compile).value,
      (bnfcGrammarDir in BNFCConfig).value,
      (bnfcOutputDir in BNFCConfig).value,
      baseDirectory.value / "src" / "main" / "k",
      baseDirectory.value / "src" / "main" / "rbl"
    ).map(_.getPath ++ "/.*").mkString(";"),
    //constrain the resource usage so that we hit SOE-s and OOME-s more quickly should they happen
    javaOptions in Test ++= Seq("-Xss240k", "-XX:MaxJavaStackTraceDepth=10000", "-Xmx128m")
  )
  .dependsOn(
    models % "compile->compile;test->test",
    rspace % "compile->compile;test->test",
    shared % "compile->compile;test->test",
    crypto
  )

lazy val rholangCLI = (project in file("rholang-cli"))
  .settings(commonSettings: _*)
  .settings(
    mainClass in assembly := Some("coop.rchain.rholang.interpreter.RholangCLI"),
    assemblyMergeStrategy in assembly := {
      case path if path.endsWith("module-info.class") => MergeStrategy.discard
      case path                                       => MergeStrategy.defaultMergeStrategy(path)
    }
  )
  .dependsOn(rholang)

lazy val blockStorage = (project in file("block-storage"))
  .settings(commonSettings: _*)
  .settings(
    name := "block-storage",
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= commonDependencies ++ protobufLibDependencies ++ Seq(
      catsCore,
      catsEffect,
      catsMtl
    )
  )
  .dependsOn(shared, models % "compile->compile;test->test")

lazy val rspace = (project in file("rspace"))
  .configs(IntegrationTest extend Test)
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(commonSettings: _*)
  .settings(
    scalacOptions ++= Seq(
      "-Xfatal-warnings"
    ),
    Defaults.itSettings,
    name := "rspace",
    version := "0.2.1-SNAPSHOT",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ Seq(
      lmdbjava,
      catsCore,
      fs2Core,
      scodecCore,
      scodecBits,
      guava
    ),
    /* Tutorial */
    /* Publishing Settings */
    scmInfo := Some(
      ScmInfo(url("https://github.com/rchain/rchain"), "git@github.com:rchain/rchain.git")
    ),
    git.remoteRepo := scmInfo.value.get.connection,
    pomIncludeRepository := { _ =>
      false
    },
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://www.rchain.coop"))
  )
  .dependsOn(shared % "compile->compile;test->test", crypto)

lazy val rspaceBench = (project in file("rspace-bench"))
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies,
    libraryDependencies += "com.esotericsoftware" % "kryo" % "5.0.3",
    dependencyOverrides ++= Seq(
      "org.ow2.asm" % "asm" % "9.0"
    ),
    sourceDirectory in Jmh := (sourceDirectory in Test).value,
    classDirectory in Jmh := (classDirectory in Test).value,
    dependencyClasspath in Jmh := (dependencyClasspath in Test).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail),
    compile in Jmh := (compile in Jmh).dependsOn(compile in Test).value,
    run in Jmh := (run in Jmh).dependsOn(Keys.compile in Jmh).evaluated
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(rspace % "test->test", rholang % "test->test", models % "test->test")

lazy val rchain = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(
    blockStorage,
    casper,
    comm,
    crypto,
    graphz,
    models,
    node,
    regex,
    rholang,
    rholangCLI,
    rspace,
    rspaceBench,
    shared
  )
