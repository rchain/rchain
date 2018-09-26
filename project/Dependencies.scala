import sbt._

object Dependencies {

  val osClassifier: String = Detector.detect(Seq("fedora")).osClassifier

  val circeVersion  = "0.10.0-M2"
  val http4sVersion = "0.19.0-M2"
  val kamonVersion  = "1.1.0"

  // format: off
  val bitcoinjCore        = "org.bitcoinj"                % "bitcoinj-core"             % "0.14.6"
  val bouncyCastle        = "org.bouncycastle"            % "bcprov-jdk15on"            % "1.59"
  val catsCore            = "org.typelevel"              %% "cats-core"                 % "1.4.0"
  val catsEffect          = "org.typelevel"              %% "cats-effect"               % "1.0.0"
  val catsMtl             = "org.typelevel"              %% "cats-mtl-core"             % "0.3.0"
  val circeCore           = "io.circe"                   %% "circe-core"                % circeVersion
  val circeGeneric        = "io.circe"                   %% "circe-generic"             % circeVersion
  val circeGenericExtras  = "io.circe"                   %% "circe-generic-extras"      % circeVersion
  val circeLiteral        = "io.circe"                   %% "circe-literal"             % circeVersion
  val circeParser         = "io.circe"                   %% "circe-parser"              % circeVersion
  val guava               = "com.google.guava"            % "guava"                     % "24.0-jre"
  val hasher              = "com.roundeights"            %% "hasher"                    % "1.2.0"
  val http4sBlazeClient   = "org.http4s"                 %% "http4s-blaze-client"       % http4sVersion
  val http4sBlazeServer   = "org.http4s"                 %% "http4s-blaze-server"       % http4sVersion
  val http4sCirce         = "org.http4s"                 %% "http4s-circe"              % http4sVersion
  val http4sDSL           = "org.http4s"                 %% "http4s-dsl"                % http4sVersion
  val jaxb                = "javax.xml.bind"              % "jaxb-api"                  % "2.1"
  val jline               = ("org.scala-lang"             % "jline"                     % "2.10.7")
    .exclude("org.fusesource.jansi", "jansi")
  // see https://jitpack.io/#rchain/kalium
  val kalium              = "com.github.rchain"           % "kalium"                    % "0.8.1"
  val kamonCore           = "io.kamon"                   %% "kamon-core"                % kamonVersion
  val kamonPrometheus     = "io.kamon"                   %% "kamon-prometheus"          % kamonVersion
  val lightningj          = ("org.lightningj"             % "lightningj"                % "0.4.2-Beta-2")
    .intransitive() //we only use the lib for one util class (org.lightningj.util.ZBase32) that has no dependencies
  val lmdbjava            = "org.lmdbjava"                % "lmdbjava"                  % "0.6.1"
  val logbackClassic      = "ch.qos.logback"              % "logback-classic"           % "1.2.3"
  val monix               = "io.monix"                   %% "monix"                     % "3.0.0-RC2-d0feeba"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging"             % "3.7.2"
  val scalaUri            = "io.lemonlabs"               %% "scala-uri"                 % "1.1.4"
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.13.5" % "test"
  val scalacheckNoTest    = "org.scalacheck"             %% "scalacheck"                % "1.13.5"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % "test"
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.0.5" % "test"
  val scalapbRuntime      = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val scalapbRuntimeLib   = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion
  val scalapbRuntimegGrpc = "com.thesamet.scalapb"       %% "scalapb-runtime-grpc"      % scalapb.compiler.Version.scalapbVersion
  val grpcNetty           = "io.grpc"                     % "grpc-netty"                % scalapb.compiler.Version.grpcJavaVersion
  val nettyBoringSsl      = "io.netty"                    % "netty-tcnative-boringssl-static" % "2.0.8.Final"
  val nettyTcnative       = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier osClassifier
  val nettyTcnativeLinux  = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier "linux-x86_64"
  val nettyTcnativeFedora = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier "linux-x86_64-fedora"
  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.0.5" % "test"
  val scallop             = "org.rogach"                 %% "scallop"                   % "3.0.3"
  val scodecCore          = "org.scodec"                 %% "scodec-core"               % "1.10.3"
  val scodecCats          = "org.scodec"                 %% "scodec-cats"               % "0.8.0"
  val scodecBits          = "org.scodec"                 %% "scodec-bits"               % "1.1.6"
  val shapeless           = "com.chuusai"                %% "shapeless"                 % "2.3.3"
  val weupnp              = "org.bitlet"                  % "weupnp"                    % "0.1.+"
  // see https://jitpack.io/#rchain/secp256k1-java
  val secp256k1Java       = "com.github.rchain"           % "secp256k1-java"            % "0.1"
  val tomlScala           = "tech.sparse"                %% "toml-scala"                % "0.1.1"
  // format: on

  val overrides = Seq(
    catsCore,
    catsEffect,
    shapeless,
    guava,
    scodecBits,
    //overrides for transitive dependencies (we don't use them directly, hence no val-s)
    "org.typelevel"            %% "machinist"              % "0.6.5",
    "com.lihaoyi"              %% "sourcecode"             % "0.1.4",
    "org.scala-lang.modules"   %% "scala-xml"              % "1.1.0",
    "com.google.code.findbugs" % "jsr305"                  % "3.0.2",
    "com.google.errorprone"    % "error_prone_annotations" % "2.1.2",
    "com.github.jnr"           % "jnr-ffi"                 % "2.1.7"
  )

  private val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

  private val testing = Seq(scalactic, scalatest, scalacheck)

  private val logging = Seq(scalaLogging, logbackClassic)

  private val circeDependencies: Seq[ModuleID] =
    Seq(circeCore, circeGeneric, circeGenericExtras, circeParser, circeLiteral)

  private val http4sDependencies: Seq[ModuleID] =
    Seq(http4sDSL, http4sBlazeServer, http4sBlazeClient, http4sCirce)

  val protobufDependencies: Seq[ModuleID] =
    Seq(scalapbRuntime)

  val protobufLibDependencies: Seq[ModuleID] =
    Seq(scalapbRuntimeLib)

  val kamonDependencies: Seq[ModuleID] =
    Seq(kamonCore, kamonPrometheus)

  val apiServerDependencies: Seq[ModuleID] =
    http4sDependencies ++ circeDependencies

  val commonDependencies: Seq[ModuleID] =
    logging ++ testing :+ kindProjector
}
