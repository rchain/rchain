import sbt._

object Dependencies {

  val osClassifier: String = Detector.detect(Seq("fedora")).osClassifier

  val circeVersion      = "0.13.0"
  val enumeratumVersion = "1.5.13"
  val http4sVersion     = "0.21.4"
  val kamonVersion      = "1.1.5"
  val catsVersion       = "2.1.0"
  val catsEffectVersion = "2.0.0"
  val catsMtlVersion    = "0.7.0"
  val slf4jVersion      = "1.7.25"

  // format: off
  val bouncyProvCastle    = "org.bouncycastle"           % "bcprov-jdk15on"             % "1.66"
  val bouncyPkixCastle    = "org.bouncycastle"           % "bcpkix-jdk15on"             % "1.66"
  val catsCore            = "org.typelevel"              %% "cats-core"                 % catsVersion
  val catsLawsTest        = "org.typelevel"              %% "cats-laws"                 % catsVersion % "test"
  val catsLawsTestkitTest = "org.typelevel"              %% "cats-testkit"              % catsVersion % "test"
  val catsEffect          = "org.typelevel"              %% "cats-effect"               % catsEffectVersion
  val catsEffectLawsTest  = "org.typelevel"              %% "cats-effect-laws"          % catsEffectVersion % "test"
  val catsMtl             = "org.typelevel"              %% "cats-mtl-core"             % catsMtlVersion
  val catsMtlLawsTest     = "org.typelevel"              %% "cats-mtl-laws"             % catsMtlVersion % "test"
  val catsTagless         = "org.typelevel"              %% "cats-tagless-macros"       % "0.11"
  val disciplineCore      = "org.typelevel"                                    %% "discipline-core"           % "1.0.2"
  val circeCore           = "io.circe"                   %% "circe-core"                % circeVersion
  val circeGeneric        = "io.circe"                   %% "circe-generic"             % circeVersion
  val circeGenericExtras  = "io.circe"                   %% "circe-generic-extras"      % circeVersion
  val circeLiteral        = "io.circe"                   %% "circe-literal"             % circeVersion
  val circeParser         = "io.circe"                   %% "circe-parser"              % circeVersion
  val enumeratum          = "com.beachape"               %% "enumeratum"                % enumeratumVersion
  val guava               = "com.google.guava"            % "guava"                     % "24.1.1-jre"
  val hasher              = "com.roundeights"            %% "hasher"                    % "1.2.0"
  val http4sBlazeClient   = "org.http4s"                 %% "http4s-blaze-client"       % http4sVersion
  val http4sBlazeServer   = "org.http4s"                 %% "http4s-blaze-server"       % http4sVersion
  val http4sCirce         = "org.http4s"                 %% "http4s-circe"              % http4sVersion
  val http4sDSL           = "org.http4s"                 %% "http4s-dsl"                % http4sVersion
  val jaxb                = "javax.xml.bind"              % "jaxb-api"                  % "2.3.1"
  val jline               = ("org.scala-lang"             % "jline"                     % "2.10.7")
    .exclude("org.fusesource.jansi", "jansi")
  // see https://jitpack.io/#rchain/kalium
  val kalium              = "com.github.rchain"           % "kalium"                    % "0.8.1"
  val kamonCore           = "io.kamon"                   %% "kamon-core"                % kamonVersion
  val kamonSystemMetrics  = "io.kamon"                   %% "kamon-system-metrics"      % "1.0.1"
  val kamonPrometheus     = "io.kamon"                   %% "kamon-prometheus"          % "1.1.2"
  val kamonInfluxDb       = "io.kamon"                   %% "kamon-influxdb"            % "1.0.2"
  val kamonZipkin         = "io.kamon"                   %% "kamon-zipkin"              % "1.0.0"
  val lightningj          = ("org.lightningj"             % "lightningj"                % "0.5.2-Beta")
    .intransitive() //we only use the lib for one util class (org.lightningj.util.ZBase32) that has no dependencies
  val lmdbjava            = "org.lmdbjava"                % "lmdbjava"                  % "0.6.3"
  val logbackClassic      = "ch.qos.logback"              % "logback-classic"           % "1.2.3"
  val lz4                 = "org.lz4"                     % "lz4-java"                  % "1.5.1"
  val monix               = "io.monix"                   %% "monix"                     % "3.1.0"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging"             % "3.9.2"
  val scalaUri            = "io.lemonlabs"               %% "scala-uri"                 % "1.1.5"
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.14.3"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % "test"
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.0.5" % "test"
  val scalapbCompiler     = "com.thesamet.scalapb"       %% "compilerplugin"            % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntime      = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val scalapbRuntimeLib   = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntimegGrpc = "com.thesamet.scalapb"       %% "scalapb-runtime-grpc"      % scalapb.compiler.Version.scalapbVersion
  val grpcNetty           = "io.grpc"                     % "grpc-netty"                % scalapb.compiler.Version.grpcJavaVersion
  val grpcServices        = "io.grpc"                     % "grpc-services"             % scalapb.compiler.Version.grpcJavaVersion
  val nettyBoringSsl      = "io.netty"                    % "netty-tcnative-boringssl-static" % "2.0.8.Final"
  val nettyTcnative       = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier osClassifier
  val nettyTcnativeLinux  = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier "linux-x86_64"
  val nettyTcnativeFedora = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier "linux-x86_64-fedora"
  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.0.5" % "test"
  val scallop             = "org.rogach"                 %% "scallop"                   % "3.1.4"
  val scodecCore          = "org.scodec"                 %% "scodec-core"               % "1.10.3"
  val scodecCats          = "org.scodec"                 %% "scodec-cats"               % "0.8.0"
  val scodecBits          = "org.scodec"                 %% "scodec-bits"               % "1.1.19"
  val shapeless           = "com.chuusai"                %% "shapeless"                 % "2.3.3"
  val magnolia            = "com.propensive"             %% "magnolia"                  % "0.12.0"
  val weupnp              = "org.bitlet"                  % "weupnp"                    % "0.1.4"
  // see https://jitpack.io/#rchain/secp256k1-java
  val secp256k1Java       = "com.github.rchain"           % "secp256k1-java"            % "0.1"
  val logstashLogback     = "net.logstash.logback"        % "logstash-logback-encoder"  % "5.3"
  val slf4j               = "org.slf4j"                   % "slf4j-api"                 % slf4jVersion
  val julToSlf4j          = "org.slf4j"                   % "jul-to-slf4j"              % slf4jVersion
  // format: on
  val pureconfig          = "com.github.pureconfig"       %% "pureconfig"               % "0.13.0"

  val overrides = Seq(
    catsCore,
    catsEffect,
    catsLawsTest,
    shapeless,
    guava,
    scodecBits,
    scalacheck,
    disciplineCore,
    //overrides for transitive dependencies (we don't use them directly, hence no val-s)
    "com.typesafe"             % "config"                 % "1.4.0",
    "org.typelevel"            %% "machinist"              % "0.6.5",
    "org.typelevel"            %% "catalysts-platform"     % "0.6",
    "com.lihaoyi"              %% "sourcecode"             % "0.1.4",
    "org.scala-lang.modules"   %% "scala-xml"              % "1.1.0",
    "com.google.code.findbugs" % "jsr305"                  % "3.0.2",
    "com.google.errorprone"    % "error_prone_annotations" % "2.1.2",
    "com.github.jnr"           % "jnr-ffi"                 % "2.1.7"
  )

  private val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")

  private val macroParadise = compilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
  )

  private val testing = Seq(scalactic, scalatest, scalacheck)

  private val logging = Seq(slf4j, julToSlf4j, scalaLogging, logbackClassic, logstashLogback)

  private val circeDependencies: Seq[ModuleID] =
    Seq(circeGeneric)

  private val http4sDependencies: Seq[ModuleID] =
    Seq(http4sDSL, http4sBlazeServer, http4sCirce)

  val protobufDependencies: Seq[ModuleID] =
    Seq(scalapbRuntime)

  val protobufLibDependencies: Seq[ModuleID] =
    Seq(scalapbRuntimeLib)

  val kamonDependencies: Seq[ModuleID] =
    Seq(kamonCore, kamonSystemMetrics, kamonPrometheus, kamonZipkin, kamonInfluxDb)

  val apiServerDependencies: Seq[ModuleID] =
    http4sDependencies ++ circeDependencies

  val commonDependencies: Seq[ModuleID] =
    logging ++ testing :+ kindProjector :+ macroParadise
}
