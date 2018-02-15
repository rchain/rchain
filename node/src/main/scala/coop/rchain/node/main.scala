package coop.rchain.node

import org.rogach.scallop._
import java.util.UUID
import java.net.{InetAddress, NetworkInterface}
import scala.collection.JavaConverters._
import coop.rchain.p2p
import coop.rchain.comm._
import com.typesafe.scalalogging.Logger

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("RChain Node version 0.1")

  val name =
    opt[String](default = None, short = 'n', descr = "Node name or key.")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val bootstrap =
    opt[String](default = Some("rnode://0f365f1016a54747b384b386b8e85352@216.83.154.106:30012"),
                short = 'b',
                descr = "Bootstrap rnode address for initial seed.")

  val standalone = opt[Boolean](default = Some(false),
                                short = 's',
                                descr = "Start a stand-alone node (no bootstrapping).")

  val host = opt[String](default = None, descr = "Hostname or IP of this node.")

  verify()
}

object Main {
  val logger = Logger("main")

  def whoami(port: Int): Option[InetAddress] = {

    val upnp = new UPnP(port)

    logger.info(s"uPnP: ${upnp.localAddress} -> ${upnp.externalAddress}")

    upnp.localAddress match {
      case Some(addy) => Some(addy)
      case None => {
        val ifaces = NetworkInterface.getNetworkInterfaces.asScala.map(_.getInterfaceAddresses)
        val addresses = ifaces
          .flatMap(_.asScala)
          .map(_.getAddress)
          .toList
          .groupBy(x => x.isLoopbackAddress || x.isLinkLocalAddress || x.isSiteLocalAddress)
        if (addresses.contains(false)) {
          Some(addresses(false).head)
        } else {
          val locals = addresses(true).groupBy(x => x.isLoopbackAddress || x.isLinkLocalAddress)
          if (locals.contains(false)) {
            Some(locals(false).head)
          } else if (locals.contains(true)) {
            Some(locals(true).head)
          } else {
            None
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {

    val conf = Conf(args)

    val name = conf.name.toOption match {
      case Some(key) => key
      case None      => UUID.randomUUID.toString.replaceAll("-", "")
    }

    val host = conf.host.toOption match {
      case Some(host) => host
      case None       => whoami(conf.port()).fold("localhost")(_.getHostAddress)
    }

    val addy = p2p.NetworkAddress.parse(s"rnode://$name@$host:${conf.port()}") match {
      case Right(node)               => node
      case Left(p2p.ParseError(msg)) => throw new Exception(msg)
    }

    // TODO consider closing this over IO
    val net = p2p.Network(addy)
    logger.info(s"Listening for traffic on $net.")

    if (!conf.standalone()) {
      conf.bootstrap.toOption.flatMap(p2p.NetworkAddress.parse _ andThen (_.toOption)).foreach {
        address =>
          logger.info(s"Bootstrapping from $address.")
          net.connect(address)
      }
    } else {
      logger.info(s"Starting stand-alone node.")
    }

    val http = HttpServer(8080)
    http.start

    sys.addShutdownHook {
      http.stop()
      net.disconnect()
      logger.info("Goodbye.")
    }

    val pauseTime = 5000L
    var lastCount = 0
    while (true) {
      Thread.sleep(pauseTime)
      for (peer <- net.net.findMorePeers(10)) {
        logger.info(s"Possibly new peer: $peer.")
        net.connect(peer)
      }
      val thisCount = net.net.table.peers.size
      if (thisCount != lastCount) {
        lastCount = thisCount
        logger.info(s"Peers: $thisCount.")
      }
    }
  }
}
