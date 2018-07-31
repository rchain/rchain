package coop.rchain.node.configuration

import java.net.InetAddress
import java.nio.file.Path

import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.CasperConf
import coop.rchain.comm.PeerNode
import coop.rchain.node.IpChecker

final class Configuration(
    val command: Command,
    val server: Server,
    val grpcServer: GrpcServer,
    val tls: Tls,
    val casper: CasperConf,
    val blockstorage: LMDBBlockStore.Config,
    private val options: commandline.Options
) {
  def printHelp(): Unit = options.printHelp()

  def fetchHost(externalAddress: Option[String]): String =
    server.host match {
      case Some(h) => h
      case None    => whoami(server.port, externalAddress)
    }

  private def check(source: String, from: String): PartialFunction[Unit, (String, String)] =
    Function.unlift(_ => IpChecker.checkFrom(from).map(ip => (source, ip)))

  private def upnpIpCheck(
      externalAddress: Option[String]): PartialFunction[Unit, (String, String)] =
    Function.unlift(_ =>
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

case class Server(
    host: Option[String],
    port: Int,
    httpPort: Int,
    metricsPort: Int,
    noUpnp: Boolean,
    defaultTimeout: Int,
    bootstrap: PeerNode,
    standalone: Boolean,
    dataDir: Path,
    mapSize: Long
)

case class GrpcServer(
    host: String,
    port: Int
)

case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean
)

sealed trait Command
case class Eval(files: List[String]) extends Command
case object Repl                     extends Command
case object Diagnostics              extends Command
case class Deploy(location: String)  extends Command
case object DeployDemo               extends Command
case object Propose                  extends Command
case class ShowBlock(hash: String)   extends Command
case object ShowBlocks               extends Command
case object Run                      extends Command
case object Help                     extends Command
