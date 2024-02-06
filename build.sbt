import Dependencies.*
import BNFC.*
import Rholang.*
import NativePackagerHelper.*
import Secp256k1.*
import com.typesafe.sbt.packager.docker.*
import protocbridge.Target
//allow stopping sbt tasks using ctrl+c without killing sbt itself
Global / cancelable := true

//disallow any unresolved version conflicts at all for faster feedback
Global / conflictManager := ConflictManager.strict
//resolve all version conflicts explicitly
Global / dependencyOverrides := Dependencies.overrides

lazy val projectSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.13.10",
  version := "0.1.0-SNAPSHOT",
  resolvers ++=
    Resolver.sonatypeOssRepos("releases") ++
      Resolver.sonatypeOssRepos("snapshots") ++
      Seq("jitpack" at "https://jitpack.io"),
  wartremoverExcluded += sourceManaged.value,
  Compile / compile / wartremoverErrors ++= Warts.allBut(
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
    Wart.ArrayEquals,
    Wart.While,
    Wart.Any,
    Wart.Product,
    Wart.Serializable,
    Wart.OptionPartial,
    Wart.Option2Iterable,
    Wart.ToString,
    Wart.MutableDataStructures,
    Wart.FinalVal,
    Wart.Null,
    Wart.AsInstanceOf,
    Wart.ExplicitImplicitTypes,
    Wart.StringPlusAny,
    Wart.AnyVal,
    // Added after migration to Scala 2.13
    Wart.TripleQuestionMark,
    Wart.IterableOps,
    Wart.JavaSerializable,
    Wart.ListUnapply,
    Wart.GlobalExecutionContext,
    Wart.NoNeedImport,
    Wart.PlatformDefault,
    Wart.JavaNetURLConstructors,
    Wart.SizeIs,
    Wart.SizeToLength,
    Wart.ListAppend,
    Wart.AutoUnboxing,
    Wart.RedundantConversions
  ),
  scalafmtOnCompile := !sys.env.contains("CI"), // disable in CI environments
  ThisBuild / scapegoatVersion := "2.1.1",
  ThisBuild / scalacOptions += "semanticdb:synthetics:on",
  Test / testOptions += Tests.Argument("-oD"), //output test durations
  javacOptions ++= Seq("-source", "11", "-target", "11"),
  Test / fork := true,
  Test / parallelExecution := false,
  Test / testForkedParallel := false,
  IntegrationTest / fork := true,
  IntegrationTest / parallelExecution := false,
  IntegrationTest / testForkedParallel := false,
  assembly / assemblyMergeStrategy := {
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
      Compile / packageDoc / publishArtifact := false,
      packageDoc / publishArtifact := false,
      Compile / doc / sources := Seq.empty
    )
  }

// a namespace for generative tests (or other tests that take a long time)
lazy val SlowcookerTest = config("slowcooker") extend (Test)

lazy val coverageSettings = Seq(
  coverageMinimum := 90,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := Seq(
    (Compile / javaSource).value,
    (Compile / sourceManaged).value.getPath ++ "/.*"
  ).mkString(";")
)

lazy val compilerSettings = CompilerSettings.options ++ Seq(
  crossScalaVersions := Seq(scalaVersion.value)
)

// Before starting sbt export YOURKIT_AGENT set to the profiling agent appropriate
// for your OS (https://www.yourkit.com/docs/java/help/agent.jsp)
lazy val profilerSettings = Seq(
  run / javaOptions ++= sys.env
    .get("YOURKIT_AGENT")
    .map(agent => s"-agentpath:$agent=onexit=snapshot,sampling")
    .toSeq,
  reStart / javaOptions ++= (run / javaOptions).value
)

lazy val commonSettings = projectSettings ++ coverageSettings ++ compilerSettings ++ profilerSettings

lazy val sdk = (project in file("sdk"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ Seq(
      catsCore,
      catsEffect,
      fs2Core
    )
  )

lazy val shared = (project in file("shared"))
  .settings(commonSettings: _*)
  .settings(
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ Seq(
      catsCore,
      catsEffect,
      catsMtl,
      fs2Core,
      lz4,
      scodecCore,
      scodecCats,
      scodecBits,
      scalapbRuntimegGrpc,
      lmdbjava,
      catsEffectLawsTest,
      catsLawsTest,
      catsLawsTestkitTest,
      enumeratum,
      jaxb
    )
  )
  .dependsOn(sdk)

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
      fs2Core,
      fs2Io,
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
  .enablePlugins(Fs2Grpc)
  .settings(commonSettings: _*)
  .settings(
    scalapbCodeGeneratorOptions += CodeGeneratorOption.FlatPackage,
    // it turns out that Fs2GrpcPlugin.autoImport.scalapbCodeGenerators.value.head is generator that is overridden by
    // StacksafeScalapbGenerator, so to resolve conflicts it is just dropped. This is found empirically, so
    // might break when upgrading the version of Fs2Grpc plugin.
    scalapbCodeGenerators := Fs2GrpcPlugin.autoImport.scalapbCodeGenerators.value.tail :+
      new Target(
        coop.rchain.scalapb.gen(flatPackage = true)._1,
        (Compile / sourceManaged).value,
        coop.rchain.scalapb.gen(flatPackage = true)._2
      ),
    version := "0.1",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ protobufDependencies ++ Seq(
      grpcNetty,
      nettyBoringSsl,
      scalapbRuntimegGrpc,
      scalaUri,
      weupnp,
      catsCore,
      catsMtl,
      guava
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
      scodecBits
    ),
    fork := true
  )
  .dependsOn(shared, secp256k1)

lazy val models = (project in file("models"))
  .settings(commonSettings: _*)
  .enablePlugins(Fs2Grpc)
  .settings(
    scalapbCodeGeneratorOptions += CodeGeneratorOption.FlatPackage,
    // it turns out that Fs2GrpcPlugin.autoImport.scalapbCodeGenerators.value.head is generator that is overridden by
    // StacksafeScalapbGenerator, so to resolve conflicts it is just dropped. This is found empirically, so
    // might break when upgrading the version of Fs2Grpc plugin.
    scalapbCodeGenerators := Fs2GrpcPlugin.autoImport.scalapbCodeGenerators.value.tail :+
      new Target(
        coop.rchain.scalapb.gen(flatPackage = true)._1,
        (Compile / sourceManaged).value,
        coop.rchain.scalapb.gen(flatPackage = true)._2
      ),
    libraryDependencies ++= commonDependencies ++ protobufDependencies ++ Seq(
      catsCore,
      magnolia,
      scalapbCompiler,
      scalacheck % "test",
      scalacheckShapeless,
      scalapbRuntimegGrpc
    )
  )
  .dependsOn(shared % "compile->compile;test->test", rspace)

lazy val node = (project in file("node"))
  .settings(commonSettings: _*)
  .enablePlugins(RpmPlugin, DebianPlugin, JavaAppPackaging, BuildInfoPlugin, Fs2Grpc)
  .settings(
    scalapbCodeGeneratorOptions += CodeGeneratorOption.FlatPackage,
    // it turns out that Fs2GrpcPlugin.autoImport.scalapbCodeGenerators.value.head is exactly the
    // generator that is overridden by StacksafeScalapbGenerator. To resolve conflicts it is just dropped.
    // This is found empirically, so might break when upgrading the version of Fs2Grpc plugin.
    // If both versions are generated, multiple copies of the same traits are produced leading to compilation error.
    scalapbCodeGenerators := Fs2GrpcPlugin.autoImport.scalapbCodeGenerators.value.tail :+
      new Target(
        coop.rchain.scalapb.gen(flatPackage = true)._1,
        (Compile / sourceManaged).value,
        coop.rchain.scalapb.gen(flatPackage = true)._2
      ),
    // if this is not specified similar error happens https://github.com/sbt/sbt-buildinfo/issues/149
    // looks like fs2 grpc plugin pipeline removes BuildInfo.scala
    PB.deleteTargetDirectory := false,
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
        catsRetry,
        grpcNetty,
        grpcServices,
        jline,
        scallop,
        scalaUri,
        scalapbRuntimegGrpc,
        circeParser,
        circeGenericExtras,
        pureconfig
      ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls" // used by org.rogach.scallop CLI args lib
    ),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, git.gitHeadCommit),
    buildInfoPackage := "coop.rchain.node",
    Compile / mainClass := Some("coop.rchain.node.Main"),
    assembly / mainClass := Some("coop.rchain.node.Main"),
    assembly / assemblyMergeStrategy := {
      case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
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
      val daemon = (Docker / daemonUser).value
      Seq(
        Cmd("FROM", dockerBaseImage.value),
        ExecCmd("RUN", "apt", "update"),
        ExecCmd("RUN", "apt", "install", "-yq", "openssl", "openssh-server", "procps"),
        Cmd("LABEL", s"""MAINTAINER="${maintainer.value}""""),
        Cmd("WORKDIR", (Docker / defaultLinuxInstallLocation).value),
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
    // Replace unsupported character `+`
    Docker / version := { version.value.replace("+", "__") },
    Docker / mappings ++= {
      val base = (Docker / defaultLinuxInstallLocation).value
      directory((rholang / baseDirectory).value / "examples")
        .map { case (f, p) => f -> s"$base/$p" }
    },
    /* Packaging */
    linuxPackageMappings ++= {
      val file = baseDirectory.value / "rnode.service"
      val rholangExamples = directory((rholang / baseDirectory).value / "examples")
        .map { case (f, p) => (f, s"/usr/share/rnode/$p") }
      Seq(
        packageMapping(file -> "/lib/systemd/system/rnode.service"),
        packageMapping(rholangExamples: _*)
      )
    },
    /* Debian */
    Debian / debianPackageDependencies ++= Seq(
      "openjdk-11-jre-headless",
      "openssl(>= 1.0.2g) | openssl(>= 1.1.1h)", //ubuntu & debian
      "bash (>= 2.05a-11)"
    ),
    /* Redhat */
    /*
     * RPM version string cannot contain dashes:
     *   http://ftp.rpm.org/max-rpm/ch-rpm-file-format.html
     */
    Rpm / version := version.value.replace('-', '.'),
    rpmVendor := "rchain.coop",
    rpmUrl := Some("https://rchain.coop"),
    rpmLicense := Some("Apache 2.0"),
    Rpm / packageArchitecture := "noarch",
    Rpm / maintainerScripts := maintainerScriptsAppendFromFile((Rpm / maintainerScripts).value)(
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
  .dependsOn(casper % "compile->compile;test->test", comm, crypto, rholang)

lazy val regex = (project in file("regex"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

lazy val rholang = (project in file("rholang"))
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)
  .settings(
    name := "rholang",
    scalacOptions ++= Seq(
      "-Xlint:-missing-interpolator" // Disable false positive strings containing ${...}
    ),
    Compile / packageDoc / publishArtifact := false,
    packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= commonDependencies ++ Seq(
      catsMtl,
      catsEffect,
      scallop,
      lightningj,
      catsLawsTest,
      catsLawsTestkitTest,
      catsMtlLawsTest
    ),
    // TODO: investigate if still needed?
    // mainClass in assembly := Some("coop.rchain.rho2rose.Rholang2RosetteCompiler"),
    coverageExcludedFiles := Seq(
      (Compile / javaSource).value,
      (BNFCConfig / bnfcGrammarDir).value,
      (BNFCConfig / bnfcOutputDir).value,
      baseDirectory.value / "src" / "main" / "k",
      baseDirectory.value / "src" / "main" / "rbl"
    ).map(_.getPath ++ "/.*").mkString(";"),
    //constrain the resource usage so that we hit SOE-s and OOME-s more quickly should they happen
    Test / javaOptions ++= Seq("-Xss240k", "-XX:MaxJavaStackTraceDepth=10000", "-Xmx128m")
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
    assembly / mainClass := Some("coop.rchain.rholang.interpreter.RholangCLI"),
    assembly / assemblyMergeStrategy := {
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
    Defaults.itSettings,
    name := "rspace",
    version := "0.2.1-SNAPSHOT",
    libraryDependencies ++= commonDependencies ++ kamonDependencies ++ Seq(
      catsCore,
      fs2Core,
      scodecCore,
      scodecBits
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
    Test / publishArtifact := false,
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
    Jmh / sourceDirectory := (Test / sourceDirectory).value,
    Jmh / classDirectory := (Test / classDirectory).value,
    Jmh / dependencyClasspath := (Test / dependencyClasspath).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail),
    Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
    Jmh / run := (Jmh / run).dependsOn(Jmh / Keys.compile).evaluated
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(rspace % "test->test", rholang % "test->test", models % "test->test")

lazy val secp256k1 = (project in file("secp256k1"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= commonDependencies :+ bouncyProvCastle,
    // this is quite hacky way to pull native libraries
    pullNative := {
      val log = streams.value.log
      val pullCached = FileFunction.cached(
        // this string does not matter, just has to some folder that is supposed to be created and
        // looked up to see if cache exists
        streams.value.cacheDirectory / "qXr7LbNp",
        inStyle = FilesInfo.hash,
        outStyle = FilesInfo.exists
      ) { (in: Set[File]) =>
        log.info("Missing Secp256k1 native library, downloading...")
        // This is the main function that does download native libs into managed resource
        pullSecp256k1(in.head)
      }(Set((Compile / resourceManaged).value))
      // Returning generated files paths is important so they are copied by sbt to classes folder
      pullCached.toSeq
    },
    // NOTE: this is not called on `compile` but when tests are called or on assembly
    Compile / resourceGenerators += pullNative.taskValue
  )

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
    shared,
    sdk,
    secp256k1
  )
