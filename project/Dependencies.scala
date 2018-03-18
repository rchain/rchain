import sbt._

object Dependencies {
  private val loggingDependencies = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    "ch.qos.logback" % "logback-classic" % "1.2.3"
  )

  private val testingDependencies = Seq(
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )

  val bitcoinj             = "org.bitcoinj"           % "bitcoinj-core"    % "0.14.6"
  val bouncyCastle         = "org.bouncycastle"       % "bcprov-jdk15on"   % "1.58"
  val kalium               = "org.abstractj.kalium"   % "kalium"           % "0.7.0"
  val argParsing           = "org.rogach"             %% "scallop"         % "3.0.3"
  val uriParsing           = "io.lemonlabs"           %% "scala-uri"       % "0.5.0"
  val uPnP                 = "org.bitlet"              % "weupnp"          % "0.1.+"
  val protobufCompiler     = "com.trueaccord.scalapb" %% "compilerplugin"  % "0.6.6"
  val protobufRuntime      = "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf"
  val cats                 = "org.typelevel"          %% "cats-core"       % "1.0.1"
  val lmdb                 = "org.lmdbjava"            % "lmdbjava"        % "0.6.0"
  val shapeless            = "com.chuusai"            %% "shapeless"       % "2.3.2"
  val monix                = "io.monix"               %% "monix"           % "3.0.0-M3"
  val guava                = "com.google.guava"       % "guava"            % "24.0-jre"
  val hasher               = "com.roundeights"        %% "hasher"          % "1.2.0"
  val scalaCheck           = "org.scalacheck"         %% "scalacheck"      % "1.13.4" % "test"
  val jaxb                 = "javax.xml.bind"         % "jaxb-api"         % "2.1"
  val scalaCheckShapeless  = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test"

  val http4sVersion = "0.18.0"
  val http4sDependencies = Seq(
    // "org.http4s" %% "rho-swagger"         % http4sVersion,
    "org.http4s" %% "http4s-dsl"          % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "org.http4s" %% "http4s-circe"        % http4sVersion)

  val circeVersion = "0.9.1"
  val circeDependencies = Seq(
    "io.circe" %% "circe-core"           % circeVersion,
    "io.circe" %% "circe-generic"        % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "io.circe" %% "circe-parser"         % circeVersion,
    "io.circe" %% "circe-literal"        % circeVersion)

  val apiServerDependencies = http4sDependencies ++ circeDependencies

  val commonDependencies   = loggingDependencies ++ testingDependencies
  val protobufDependencies = Seq(protobufCompiler, protobufRuntime)

  val kamonDependencies = Seq(
    "io.kamon" %% "kamon-core"       % "1.0.0",
    "io.kamon" %% "kamon-prometheus" % "1.0.0"
  )

}
