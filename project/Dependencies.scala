import sbt._

object Dependencies {

  val osClassifier: String = Detector.detect(Seq("fedora")).osClassifier

  val catsVersion       = "2.7.0"
  val catsEffectVersion = "2.5.4"
  val catsMtlVersion    = "0.7.1"
  val fs2Version        = "2.5.10"
  val monixVersion      = "3.4.0"
  val http4sVersion     = "0.21.24"
  val endpointsVersion  = "1.4.0"
  val circeVersion      = "0.13.0"
  val enumeratumVersion = "1.5.13"
  val slf4jVersion      = "1.7.30"
  val kamonVersion      = "1.1.6"

  // format: off
  val bouncyProvCastle    = "org.bouncycastle"            % "bcprov-jdk15on"            % "1.68"
  val bouncyPkixCastle    = "org.bouncycastle"            % "bcpkix-jdk15on"            % "1.68"
  val catsCore            = "org.typelevel"              %% "cats-core"                 % catsVersion
  val catsLawsTest        = "org.typelevel"              %% "cats-laws"                 % catsVersion % "test"
  val catsLawsTestkitTest = "org.typelevel"              %% "cats-testkit"              % catsVersion % "test"
  val catsEffect          = "org.typelevel"              %% "cats-effect"               % catsEffectVersion
  val catsEffectLawsTest  = "org.typelevel"              %% "cats-effect-laws"          % catsEffectVersion % "test"
  val catsMtl             = "org.typelevel"              %% "cats-mtl-core"             % catsMtlVersion
  val catsMtlLawsTest     = "org.typelevel"              %% "cats-mtl-laws"             % catsMtlVersion % "test"
  val catsRetry           = "com.github.cb372"           %% "cats-retry"                % "2.1.0"
  val catsTagless         = "org.typelevel"              %% "cats-tagless-macros"       % "0.14.0"
  val circeCore           = "io.circe"                   %% "circe-core"                % circeVersion
  val circeGeneric        = "io.circe"                   %% "circe-generic"             % circeVersion
  val circeGenericExtras  = "io.circe"                   %% "circe-generic-extras"      % circeVersion
  val circeLiteral        = "io.circe"                   %% "circe-literal"             % circeVersion
  val circeParser         = "io.circe"                   %% "circe-parser"              % circeVersion
  val disciplineCore      = "org.typelevel"              %% "discipline-core"           % "1.4.0"
  val enumeratum          = "com.beachape"               %% "enumeratum"                % enumeratumVersion
  val endpoints           = "org.endpoints4s"            %% "algebra"                   % endpointsVersion
  val endpointsAlgCirce   = "org.endpoints4s"            %% "algebra-circe"             % endpointsVersion
  val endpointsAlgJson    = "org.endpoints4s"            %% "algebra-json-schema"       % endpointsVersion
  val endpointsGeneric    = "org.endpoints4s"            %% "json-schema-generic"       % endpointsVersion
  val endpointsCirce      = "org.endpoints4s"            %% "json-schema-circe"         % endpointsVersion
  val endpointsHttp4s     = "org.endpoints4s"            %% "http4s-server"             % "6.0.0"
  val endpointsOpenApi    = "org.endpoints4s"            %% "openapi"                   % "3.0.0"
  val fs2Core             = "co.fs2"                     %% "fs2-core"                  % fs2Version
  val fs2Io               = "co.fs2"                     %% "fs2-io"                    % fs2Version
  val guava               = "com.google.guava"            % "guava"                     % "30.1-jre"
  val hasher              = "com.roundeights"            %% "hasher"                    % "1.2.0"
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
  val kamonSystemMetrics  = "io.kamon"                   %% "kamon-system-metrics"      % "1.0.1"
  val kamonPrometheus     = "io.kamon"                   %% "kamon-prometheus"          % "1.1.2"
  val kamonInfluxDb       = "io.kamon"                   %% "kamon-influxdb"            % "1.0.2"
  val kamonZipkin         = "io.kamon"                   %% "kamon-zipkin"              % "1.0.0"
  val lightningj          = ("org.lightningj"             % "lightningj"                % "0.5.2-Beta")
    .intransitive() //we only use the lib for one util class (org.lightningj.util.ZBase32) that has no dependencies
  val lmdbjava            = "org.lmdbjava"                % "lmdbjava"                  % "0.8.1"
  val logbackClassic      = "ch.qos.logback"              % "logback-classic"           % "1.2.3"
  val logstashLogback     = "net.logstash.logback"        % "logstash-logback-encoder"  % "6.6"
  val lz4                 = "org.lz4"                     % "lz4-java"                  % "1.7.1"
  val magnolia            = "com.propensive"             %% "magnolia"                  % "0.17.0"
  val monix               = "io.monix"                   %% "monix"                     % monixVersion
  val monixTesting        = "io.monix"                   %% "monix-testing-scalatest"   % "0.3.0"
  val pureconfig          = "com.github.pureconfig"      %% "pureconfig"                % "0.14.0"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging"             % "3.9.4"
  val scalaUri            = "io.lemonlabs"               %% "scala-uri"                 % "3.0.0"
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.15.0"
  val scalacheckEffect    = "org.typelevel"              %% "scalacheck-effect"         % "1.0.4"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0"  % "test"
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.2.9" % "test"
  val scalapbCompiler     = "com.thesamet.scalapb"       %% "compilerplugin"            % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntime      = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val scalapbRuntimeLib   = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntimegGrpc = "com.thesamet.scalapb"       %% "scalapb-runtime-grpc"      % scalapb.compiler.Version.scalapbVersion
  val grpcNetty           = "io.grpc"                     % "grpc-netty"                % scalapb.compiler.Version.grpcJavaVersion
  val grpcServices        = "io.grpc"                     % "grpc-services"             % scalapb.compiler.Version.grpcJavaVersion
  val nettyBoringSsl      = "io.netty"                    % "netty-tcnative-boringssl-static" % "2.0.36.Final"
  val nettyTcnative       = "io.netty"                    % "netty-tcnative"            % "2.0.36.Final" classifier osClassifier
  val nettyTcnativeLinux  = "io.netty"                    % "netty-tcnative"            % "2.0.36.Final" classifier "linux-x86_64"
  val nettyTcnativeFedora = "io.netty"                    % "netty-tcnative"            % "2.0.36.Final" classifier "linux-x86_64-fedora"
  val scalaCompat         = "org.scala-lang.modules"     %% "scala-collection-compat"   % "2.6.0"
  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.2.9"   % "test"
  val scalatestPlus       = "org.scalatestplus"          %% "scalacheck-1-15"           % "3.2.9.0" % "test"
  val scallop             = "org.rogach"                 %% "scallop"                   % "3.1.4"
  val scodecCore          = "org.scodec"                 %% "scodec-core"               % "1.11.7"
  val scodecCats          = "org.scodec"                 %% "scodec-cats"               % "1.1.0-M4"
  val scodecBits          = "org.scodec"                 %% "scodec-bits"               % "1.1.23"
  // see https://jitpack.io/#rchain/secp256k1-java
  val secp256k1Java       = "com.github.rchain"           % "secp256k1-java"            % "0.1"
  val shapeless           = "com.chuusai"                %% "shapeless"                 % "2.3.8"
  val slf4j               = "org.slf4j"                   % "slf4j-api"                 % slf4jVersion
  val weupnp              = "org.bitlet"                  % "weupnp"                    % "0.1.4"
  // format: on

  val overrides = Seq(
    catsCore,
    catsEffect,
    catsLawsTest,
    fs2Core,
    fs2Io,
    guava,
    shapeless,
    scalacheck,
    scodecBits,
    scalaCompat,
    slf4j,
    kamonCore,
    "org.typelevel" % "jawn-parser_2.12" % "1.0.1",
    // Added to resolve conflicts in scalapb plugin v0.10.8
    "com.google.protobuf" % "protobuf-java" % "3.12.0",
    // Strange version conflict, it requires the same version but in square brackets (range?).
    // e.g. io.grpc:grpc-core:1.30.2 ([1.30.2] wanted)
    // https://stackoverflow.com/questions/59423185/strange-versions-conflict-in-sbt-strict-mode
    "io.grpc"  % "grpc-api"          % "1.30.2",
    "io.grpc"  % "grpc-core"         % "1.30.2",
    "io.netty" % "netty-codec-http2" % "4.1.48.Final",
    // Overrides for transitive dependencies (we don't use them directly, hence no val-s)
    "com.github.jnr"         % "jnr-ffi"     % "2.1.15",
    "com.lihaoyi"            %% "geny"       % "0.6.10",
    "com.lihaoyi"            %% "sourcecode" % "0.2.1",
    "org.scala-lang.modules" %% "scala-xml"  % "1.3.0",
    "com.typesafe"           % "config"      % "1.4.0"
  )

  private val kindProjector = compilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full
  )

  // In Scala 2.13, the plugin's functionality has been included in the compiler directly under the -Ymacro-annotations flag.
  // https://github.com/scalamacros/paradise
  private val macroParadise = compilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
  )

  private val testing =
    Seq(monixTesting, scalacheck, scalacheckEffect, scalactic, scalatest, scalatestPlus)

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
    logging ++ testing :+ kindProjector :+ macroParadise :+ scalaCompat
}
