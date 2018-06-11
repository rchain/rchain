import sbt._

object Dependencies {

  val osClassifier: String = Detector.detect(Seq("fedora")).osClassifier

  val circeVersion  = "0.9.1"
  val http4sVersion = "0.18.0"
  val kamonVersion  = "1.0.0"

  // format: off
  val bitcoinjCore        = "org.bitcoinj"                % "bitcoinj-core"             % "0.14.6"
  val bouncyCastle        = "org.bouncycastle"            % "bcprov-jdk15on"            % "1.59"
  val catsCore            = "org.typelevel"              %% "cats-core"                 % "1.0.1"
  val catsMtl             = "org.typelevel"              %% "cats-mtl-core"             % "0.2.1"
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
  val jline               = ("org.scala-lang"             % "jline"                      % "2.10.7").exclude("org.fusesource.jansi", "jansi")
  val kalium              = "org.abstractj.kalium"        % "kalium"                    % "0.7.0"
  val kamonCore           = "io.kamon"                   %% "kamon-core"                % kamonVersion
  val kamonPrometheus     = "io.kamon"                   %% "kamon-prometheus"          % kamonVersion
  val lmdbjava            = "org.lmdbjava"                % "lmdbjava"                  % "0.6.0"
  val logbackClassic      = "ch.qos.logback"              % "logback-classic"           % "1.2.3"
  val monix               = "io.monix"                   %% "monix"                     % "3.0.0-RC1"
  val scalaLogging        = "com.typesafe.scala-logging" %% "scala-logging"             % "3.7.2"
  val scalaUri            = "io.lemonlabs"               %% "scala-uri"                 % "0.5.0"
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.13.4" % "test"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % "test"
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.0.1" % "test"
  val scalapbRuntime      = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val scalapbRuntimegGrpc = "com.thesamet.scalapb"       %% "scalapb-runtime-grpc"      % scalapb.compiler.Version.scalapbVersion
  val grpcNetty           = "io.grpc"                     % "grpc-netty"                % scalapb.compiler.Version.grpcJavaVersion
  val nettyBoringSsl      = "io.netty"                    % "netty-tcnative-boringssl-static" % "2.0.8.Final"
  val nettyTcnative       = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier osClassifier
  val nettyTcnativeLinux  = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier "linux-x86_64"
  val nettyTcnativeFedora = "io.netty"                    % "netty-tcnative"            % "2.0.8.Final" classifier "linux-x86_64-fedora"
  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.0.5" % "test"
  val scallop             = "org.rogach"                 %% "scallop"                   % "3.0.3"
  val scodecCore          = "org.scodec"                 %% "scodec-core"               % "1.10.3"
  val scodecCats          = "org.scodec"                 %% "scodec-cats"               % "0.6.0"
  val scodecBits          = "org.scodec"                 %% "scodec-bits"               % "1.1.5"
  val shapeless           = "com.chuusai"                %% "shapeless"                 % "2.3.2"
  val weupnp              = "org.bitlet"                  % "weupnp"                    % "0.1.+"

  // format: on

  private val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

  private val testing = Seq(scalactic, scalatest, scalacheck)

  private val logging = Seq(scalaLogging, logbackClassic)

  private val circeDependencies: Seq[ModuleID] =
    Seq(circeCore, circeGeneric, circeGenericExtras, circeParser, circeLiteral)

  private val http4sDependencies: Seq[ModuleID] =
    Seq(http4sDSL, http4sBlazeServer, http4sBlazeClient, http4sCirce)

  val protobufDependencies: Seq[ModuleID] =
    Seq(scalapbRuntime)

  val kamonDependencies: Seq[ModuleID] =
    Seq(kamonCore, kamonPrometheus)

  val apiServerDependencies: Seq[ModuleID] =
    http4sDependencies ++ circeDependencies

  val commonDependencies: Seq[ModuleID] =
    logging ++ testing :+ kindProjector
}
