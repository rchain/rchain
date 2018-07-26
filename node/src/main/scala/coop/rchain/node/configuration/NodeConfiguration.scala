package coop.rchain.node.configuration

import java.net.InetAddress
import java.nio.file.{Path, Paths}

import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.IpChecker
import coop.rchain.node.configuration.toml.TomlRoot

import org.rogach.scallop.ScallopOption

object NodeConfiguration {

  val dockerProfile =
    Profile("docker", dataDir = (() => Paths.get("/var/lib/rnode"), "Defaults to /var/lib/rnode"))

  val defaultProfile =
    Profile("default",
            dataDir =
              (() => Paths.get(sys.props("user.home"), ".rnode"), "Defaults to $HOME/.rnode"))

  val profiles: Map[String, Profile] =
    Map(defaultProfile.name -> defaultProfile, dockerProfile.name -> dockerProfile)

  val DefaultPort                  = 40400
  val DefaultGrpcPort              = 40401
  val DefaultHttPort               = 40402
  val DefaultMetricsPort           = 40403
  val DefaultGrpcHost              = "localhost"
  val DefaultNoUpNP                = false
  val DefaultStandalone            = false
  val DefaultTimeout               = 1000
  val DefaultMapSize: Long         = 1024L * 1024L * 1024L
  val DefaultNumValidators         = 5
  val DefaultValidatorSigAlgorithm = "ed25519"
  val DefaultCertificateFileName   = "node.certificate.pem"
  val DefaultKeyFileName           = "node.key.pem"

  val DefaultBootstrapServer: PeerNode = {
    val Right(bs) = PeerNode
      .parse("rnode://acd0b05a971c243817a0cfd469f5d1a238c60294@52.119.8.109:40400")
    bs
  }

  def apply(arguments: Seq[String]): Configuration = {
    val options    = commandline.Options(arguments)
    val profile    = options.profile.toOption.flatMap(profiles.get).getOrElse(defaultProfile)
    val dataDir    = options.run.data_dir.getOrElse(profile.dataDir._1())
    val configFile = dataDir.resolve("rnode.toml").toFile
    println(s"Using configuration file: $configFile")
    val config = toml.TomlConfiguration.from(configFile) match {
      case Left(error) =>
        println(s"Can't load the configuration file: $error")
        None
      case Right(c) => Some(c)
    }

    val command: Command = options.subcommand match {
      case Some(options.eval)        => Eval(options.eval.fileNames())
      case Some(options.repl)        => Repl
      case Some(options.diagnostics) => Diagnostics
      case Some(options.deploy)      => Deploy(options.deploy.location())
      case Some(options.deployDemo)  => DeployDemo
      case Some(options.propose)     => Propose
      case Some(options.showBlock)   => ShowBlock(options.showBlock.hash())
      case Some(options.showBlocks)  => ShowBlocks
      case Some(options.run)         => Run
      case _                         => Help
    }

    def getOpt[A](o: ScallopOption[A], fc: TomlRoot => Option[A]): Option[A] =
      o.toOption.orElse(config.flatMap(fc))

    def get[A](o: ScallopOption[A], fc: TomlRoot => Option[A], default: A): A =
      getOpt(o, fc).getOrElse(default)

    // gRPC
    val grpcHost: String = get(options.grpcHost, _.grpcServer.flatMap(_.host), DefaultGrpcHost)
    val grpcPort: Int    = get(options.grpcPort, _.grpcServer.flatMap(_.port), DefaultGrpcPort)

    // Server
    val port: Int     = get(options.run.port, _.server.flatMap(_.port), DefaultPort)
    val httpPort: Int = get(options.run.httpPort, _ => None, DefaultHttPort)
    val metricsPort: Int =
      get(options.run.metricsPort, _.server.flatMap(_.metricsPort), DefaultMetricsPort)
    val noUpnp: Boolean = get(options.run.noUpnp, _.server.flatMap(_.noUpnp), DefaultNoUpNP)
    val defaultTimeout: Int =
      get(options.run.defaultTimeout, _.server.flatMap(_.defaultTimeout), DefaultTimeout)
    val bootstrap: PeerNode =
      get(options.run.bootstrap, _.server.flatMap(_.bootstrap), DefaultBootstrapServer)
    val standalone: Boolean =
      get(options.run.standalone, _.server.flatMap(_.standalone), DefaultStandalone)
    val host: Option[String] = getOpt(options.run.host, _.server.flatMap(_.host))
    val mapSize: Long =
      get(options.run.map_size, _.server.flatMap(_.mapSize), DefaultMapSize)

    // TLS
    val certificate: Option[Path] = getOpt(options.run.certificate, _.tls.flatMap(_.certificate))
    val key: Option[Path]         = getOpt(options.run.key, _.tls.flatMap(_.key))

    val certificatePath: Path =
      certificate.getOrElse(dataDir.resolve(DefaultCertificateFileName))

    val keyPath: Path =
      key.getOrElse(dataDir.resolve(DefaultKeyFileName))

    // Validators
    val numValidators =
      get(options.run.numValidators, _.validators.flatMap(_.count), DefaultNumValidators)
    val bondsFile       = getOpt(options.run.bondsFile, _.validators.flatMap(_.bondsFile))
    val knownValidators = getOpt(options.run.knownValidators, _.validators.flatMap(_.known))
    val validatorPublicKey =
      getOpt(options.run.validatorPublicKey, _.validators.flatMap(_.publicKey))
    val validatorPrivateKey =
      getOpt(options.run.validatorPrivateKey, _.validators.flatMap(_.privateKey))
    val validatorSigAlgorithm =
      get(options.run.validatorSigAlgorithm,
          _.validators.flatMap(_.sigAlgorithm),
          DefaultValidatorSigAlgorithm)
    val walletsFile: Option[String] =
      getOpt(options.run.walletsFile, _.validators.flatMap(_.walletsFile))

    val server = Server(
      host,
      port,
      httpPort,
      metricsPort,
      noUpnp,
      defaultTimeout,
      bootstrap,
      standalone,
      dataDir,
      mapSize
    )
    val grpcServer = GrpcServer(
      grpcHost,
      grpcPort
    )
    val tls = Tls(
      certificatePath,
      keyPath,
      certificate.isDefined,
      key.isDefined
    )
    val casper =
      CasperConf(
        validatorPublicKey,
        validatorPrivateKey,
        validatorSigAlgorithm,
        bondsFile,
        knownValidators,
        numValidators,
        dataDir.resolve("genesis"),
        walletsFile,
        standalone
      )

    new Configuration(
      profile.name,
      command,
      server,
      grpcServer,
      tls,
      casper
    ) {
      def printHelp(): Unit = options.printHelp()

      def fetchHost(externalAddress: Option[String]): String =
        server.host match {
          case Some(h) => h
          case None    => whoami(port, externalAddress)
        }
    }
  }

  private def check(source: String, from: String): PartialFunction[Unit, (String, String)] =
    Function.unlift(Unit => IpChecker.checkFrom(from).map(ip => (source, ip)))

  private def upnpIpCheck(
      externalAddress: Option[String]): PartialFunction[Unit, (String, String)] =
    Function.unlift(Unit =>
      externalAddress.map(addy => ("UPnP", InetAddress.getByName(addy).getHostAddress)))

  private def checkAll(externalAddress: Option[String]): (String, String) = {
    val func: PartialFunction[Unit, (String, String)] =
      check("AmazonAWS service", "http://checkip.amazonaws.com") orElse
        check("WhatIsMyIP service", "http://bot.whatismyipaddress.com") orElse
        upnpIpCheck(externalAddress) orElse { case _ => ("failed to guess", "localhost") }

    func.apply(())
  }

  private def whoami(port: Int, externalAddress: Option[String]): String = {
    println("INFO - flag --host was not provided, guessing your external IP address")
    val (source, ip) = checkAll(externalAddress)
    println(s"INFO - guessed $ip from source: $source")
    ip
  }

}

case class Profile(name: String, dataDir: (() => Path, String))
