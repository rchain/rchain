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

  val bouncyCastle         = "org.bouncycastle"       % "bcprov-jdk15on"   % "1.58"
  val kalium               = "org.abstractj.kalium"   % "kalium"           % "0.7.0"
  val argParsing           = "org.rogach"             %% "scallop"         % "3.0.3"
  val uriParsing           = "io.lemonlabs"           %% "scala-uri"       % "0.5.0"
  val uPnP                 = "org.bitlet"             % "weupnp"           % "0.1.+"
  val protobufCompiler     = "com.trueaccord.scalapb" %% "compilerplugin"  % "0.6.6"
  val protobufRuntime      = "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf"
  val cats                 = "org.typelevel"          %% "cats-core"       % "1.0.1"
  val lmdb                 = "org.lmdbjava"           % "lmdbjava"         % "0.6.0"
  val shapeless            = "com.chuusai"            %% "shapeless"       % "2.3.2"
  val scalaCheck           = "org.scalacheck"         %% "scalacheck"      % "1.13.4" % "test"
  val guava                = "com.google.guava"       % "guava"            % "16.0"
  val jaxb                 = "javax.xml.bind"         % "jaxb-api"         % "2.1"

  val commonDependencies   = loggingDependencies ++ testingDependencies
  val protobufDependencies = Seq(protobufCompiler, protobufRuntime)
}
