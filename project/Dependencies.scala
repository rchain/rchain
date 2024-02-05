import sbt._

object Dependencies {

  val osClassifier: String = Detector.detect(Seq("fedora")).osClassifier

  val catsVersion       = "2.9.0"
  val catsEffectVersion = "3.4.8"
  val catsMtlVersion    = "0.7.1"
  val fs2Version        = "3.6.1"
  val http4sVersion     = "0.23.6"
  val endpointsVersion  = "1.9.0"
  val circeVersion      = "0.14.3"
  val enumeratumVersion = "1.5.13"
  val slf4jVersion      = "2.0.7"
  val kamonVersion      = "2.6.0"

  // format: off
  val bouncyProvCastle    = "org.bouncycastle"            % "bcprov-jdk15on"            % "1.68"
  val bouncyPkixCastle    = "org.bouncycastle"            % "bcpkix-jdk15on"            % "1.68"
  val catsCore            = "org.typelevel"              %% "cats-core"                 % catsVersion
  val catsLawsTest        = "org.typelevel"              %% "cats-laws"                 % catsVersion % "test"
  val catsLawsTestkitTest = "org.typelevel"              %% "cats-testkit"              % catsVersion % "test"
  val catsEffect          = "org.typelevel"              %% "cats-effect"               % catsEffectVersion
  val catsEffectStd       = "org.typelevel"              %% "cats-effect-std"           % catsEffectVersion
  val catsEffectKernel    = "org.typelevel"              %% "cats-effect-kernel"        % catsEffectVersion
  val catsEffectLawsTest  = "org.typelevel"              %% "cats-effect-laws"          % catsEffectVersion % "test"
  val catsTestControl     = "org.typelevel"              %% "cats-effect-testkit"       % catsEffectVersion % Test
  val catsMtl             = "org.typelevel"              %% "cats-mtl-core"             % catsMtlVersion
  val catsMtlLawsTest     = "org.typelevel"              %% "cats-mtl-laws"             % catsMtlVersion % "test"
  val catsRetry           = "com.github.cb372"           %% "cats-retry"                % "2.1.0"
  val circeCore           = "io.circe"                   %% "circe-core"                % circeVersion
  val circeGeneric        = "io.circe"                   %% "circe-generic"             % circeVersion
  val circeGenericExtras  = "io.circe"                   %% "circe-generic-extras"      % circeVersion
  val circeLiteral        = "io.circe"                   %% "circe-literal"             % circeVersion
  val circeParser         = "io.circe"                   %% "circe-parser"              % "0.14.1"
  val disciplineCore      = "org.typelevel"              %% "discipline-core"           % "1.4.0"
  val enumeratum          = "com.beachape"               %% "enumeratum"                % enumeratumVersion
  val endpoints           = "org.endpoints4s"            %% "algebra"                   % endpointsVersion
  val endpointsAlgCirce   = "org.endpoints4s"            %% "algebra-circe"             % "2.3.0"
  val endpointsAlgJson    = "org.endpoints4s"            %% "algebra-json-schema"       % endpointsVersion
  val endpointsGeneric    = "org.endpoints4s"            %% "json-schema-generic"       % endpointsVersion
  val endpointsCirce      = "org.endpoints4s"            %% "json-schema-circe"         % "2.3.0"
  val endpointsHttp4s     = "org.endpoints4s"            %% "http4s-server"             % "10.1.0"
  val endpointsOpenApi    = "org.endpoints4s"            %% "openapi"                   % "4.3.0"
  val fs2Core             = "co.fs2"                     %% "fs2-core"                  % fs2Version
  val fs2Io               = "co.fs2"                     %% "fs2-io"                    % fs2Version
  val guava               = "com.google.guava"            % "guava"                     % "31.1-jre"
  val http4sBlazeClient   = "org.http4s"                 %% "http4s-blaze-client"       % http4sVersion
  val http4sBlazeServer   = "org.http4s"                 %% "http4s-blaze-server"       % http4sVersion
  val http4sCirce         = "org.http4s"                 %% "http4s-circe"              % http4sVersion
  val http4sDSL           = "org.http4s"                 %% "http4s-dsl"                % http4sVersion
  val jaxb                = "javax.xml.bind"              % "jaxb-api"                  % "2.3.1"
  val jline               = ("org.scala-lang"             % "jline"                     % "2.10.7")
    .exclude("org.fusesource.jansi", "jansi")
  val julToSlf4j          = "org.slf4j"                   % "jul-to-slf4j"              % slf4jVersion
  // see https://jitpack.io/#rchain/kalium
  val kalium              = "com.github.rchain"           % "kalium"                    % "0.8.1"
  val kamonCore           = "io.kamon"                   %% "kamon-core"                % kamonVersion
  val kamonSystemMetrics  = "io.kamon"                   %% "kamon-system-metrics"      % kamonVersion
  val kamonPrometheus     = "io.kamon"                   %% "kamon-prometheus"          % kamonVersion
  val kamonInfluxDb       = "io.kamon"                   %% "kamon-influxdb"            % kamonVersion
  val kamonZipkin         = "io.kamon"                   %% "kamon-zipkin"              % kamonVersion
  val lightningj          = ("org.lightningj"             % "lightningj"                % "0.5.2-Beta")
    .intransitive() //we only use the lib for one util class (org.lightningj.util.ZBase32) that has no dependencies
  val lmdbjava            = "org.lmdbjava"                % "lmdbjava"                  % "0.9.0"
  val logbackClassic      = "ch.qos.logback"              % "logback-classic"           % "1.4.6"
  val logstashLogback     = "net.logstash.logback"        % "logstash-logback-encoder"  % "6.6"
  val lz4                 = "org.lz4"                     % "lz4-java"                  % "1.7.1"
  val magnolia            = "com.propensive"             %% "magnolia"                  % "0.17.0"
  val mockito             = "org.mockito"                %% "mockito-scala-cats"        % "1.17.14" % "test"
  val monixTesting        = "io.monix"                   %% "monix-testing-scalatest"   % "0.3.0"
  val ceTesting           = "org.typelevel"              %% "cats-effect-testing-scalatest"% "1.5.0" % Test
  val pureconfig          = "com.github.pureconfig"      %% "pureconfig"                % "0.14.0"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging"             % "3.9.4"
  val scalaUri            = "io.lemonlabs"               %% "scala-uri"                 % "3.0.0"
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.17.0"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.16" % "1.3.1"  % "test"
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.2.13" % "test"
  val scalapbCompiler     = "com.thesamet.scalapb"       %% "compilerplugin"            % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntime      = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val scalapbRuntimeLib   = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntimegGrpc = "com.thesamet.scalapb"       %% "scalapb-runtime-grpc"      % scalapb.compiler.Version.scalapbVersion
  val grpcNetty           = "io.grpc"                     % "grpc-netty"                % scalapb.compiler.Version.grpcJavaVersion
  val grpcServices        = "io.grpc"                     % "grpc-services"             % scalapb.compiler.Version.grpcJavaVersion
  val nettyBoringSsl      = "io.netty"                    % "netty-tcnative-boringssl-static" % "2.0.46.Final"
  val nettyTcnative       = "io.netty"                    % "netty-tcnative"            % "2.0.46.Final" classifier osClassifier
  val nettyTcnativeLinux  = "io.netty"                    % "netty-tcnative"            % "2.0.46.Final" classifier "linux-x86_64"
  val nettyTcnativeFedora = "io.netty"                    % "netty-tcnative"            % "2.0.46.Final" classifier "linux-x86_64-fedora"
  val scalaCompat         = "org.scala-lang.modules"     %% "scala-collection-compat"   % "2.6.0"
  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.2.13"  % "test"
  val scalatestPlus       = "org.scalatestplus"          %% "scalacheck-1-16"           % "3.2.13.0" % "test"
  val scallop             = "org.rogach"                 %% "scallop"                   % "3.3.2"
  // see https://jitpack.io/#rchain/secp256k1-java
  val secp256k1Java       = "com.github.rchain"           % "secp256k1-java"            % "0.1"
  val scodecCore          = "org.scodec"                 %% "scodec-core"               % "1.11.10"
  val scodecCats          = "org.scodec"                 %% "scodec-cats"               % "1.2.0"
  val scodecBits          = "org.scodec"                 %% "scodec-bits"               % "1.1.37"
  val shapeless           = "com.chuusai"                %% "shapeless"                 % "2.3.10"
  val slf4j               = "org.slf4j"                   % "slf4j-api"                 % slf4jVersion
  val weupnp              = "org.bitlet"                  % "weupnp"                    % "0.1.4"
  val sourcecode          = "com.lihaoyi"                %% "sourcecode"                % "0.2.1"
  val grpcNettyShaded     = "io.grpc"                     % "grpc-netty-shaded"         % scalapb.compiler.Version.grpcJavaVersion

  // format: on

  val overrides = Seq(
    catsCore,
    catsEffect,
    catsLawsTest,
    catsEffectStd,
    catsEffectKernel,
    fs2Core,
    fs2Io,
    guava,
    shapeless,
    scalacheck,
    scodecBits,
    scalaCompat,
    slf4j,
    kamonCore,
    sourcecode,
    scalatest,
    // Overrides for transitive dependencies (we don't use them directly, hence no val-s),
    "com.squareup.okhttp3"   % "okhttp"           % "3.12.1",
    "org.objenesis"          % "objenesis"        % "3.2",
    "org.typelevel"          % "jawn-parser_2.13" % "1.1.2",
    "com.github.jnr"         % "jnr-ffi"          % "2.2.13",
    "com.lihaoyi"            %% "geny"            % "1.0.0",
    "org.scala-lang.modules" %% "scala-xml"       % "2.1.0",
    "com.typesafe"           % "config"           % "1.4.2",
    // Added to resolve conflicts in scalapb plugin v0.11.3
    "com.google.code.gson"  % "gson"                       % "2.10.1",
    "com.google.protobuf"   % "protobuf-java"              % "3.12.2",
    "com.google.errorprone" % "error_prone_annotations"    % "2.18.0",
    "io.perfmark"           % "perfmark-api"               % "0.23.0",
    "org.codehaus.mojo"     % "animal-sniffer-annotations" % "1.19",
    "io.circe"              %% "circe-jawn"                % "0.14.1",
    "io.circe"              %% "circe-core"                % "0.14.1",
    "com.comcast"           %% "ip4s-core"                 % "3.0.4",
    "org.typelevel"         %% "cats-free"                 % "2.9.0",
    "org.typelevel"         %% "literally"                 % "1.0.2",
    // Strange version conflict, it requires the same version but in square brackets (range?).
    // e.g. io.grpc:grpc-core:1.37.0 ([1.37.0] wanted)
    // https://stackoverflow.com/questions/59423185/strange-versions-conflict-in-sbt-strict-mode
    "io.grpc" % "grpc-api"  % scalapb.compiler.Version.grpcJavaVersion,
    "io.grpc" % "grpc-core" % scalapb.compiler.Version.grpcJavaVersion
  )

  private val kindProjector = compilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full
  )

  private val testing =
    Seq(scalactic, scalatest, scalacheck, scalatestPlus, mockito, ceTesting, catsTestControl)

  private val logging = Seq(slf4j, julToSlf4j, scalaLogging, logbackClassic, logstashLogback)

  private val circeDependencies: Seq[ModuleID] =
    Seq(circeGeneric, circeParser)

  private val endpointsDependencies: Seq[ModuleID] =
    Seq(
      endpoints,
      endpointsAlgCirce,
      endpointsAlgJson,
      endpointsGeneric,
      endpointsCirce,
      endpointsHttp4s,
      endpointsOpenApi
    )

  private val http4sDependencies: Seq[ModuleID] =
    Seq(http4sDSL, http4sBlazeServer, http4sCirce) ++ endpointsDependencies

  val protobufDependencies: Seq[ModuleID] =
    Seq(scalapbRuntime)

  val protobufLibDependencies: Seq[ModuleID] =
    Seq(scalapbRuntimeLib)

  val kamonDependencies: Seq[ModuleID] =
    Seq(kamonCore, kamonSystemMetrics, kamonPrometheus, kamonZipkin, kamonInfluxDb)

  val apiServerDependencies: Seq[ModuleID] =
    http4sDependencies ++ circeDependencies

  val commonDependencies: Seq[ModuleID] =
    logging ++ testing :+ kindProjector :+ scalaCompat :+ sourcecode
}
