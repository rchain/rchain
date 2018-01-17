package coop.rchain.comm

import org.rogach.scallop._
import java.util.UUID
import java.nio.ByteBuffer
import java.net.{InetAddress, NetworkInterface}
import scala.collection.JavaConverters._
// import scala.collection.JavaConversions._
import coop.rchain.p2p
import com.typesafe.scalalogging.Logger

case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("RChain Communications Library version 0.1")

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

  /*
   * Duration (in ms) to pause between successive queries for more peers.
   */
  val pauseTime = 5000L

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
          if (addresses.contains(false)) {
            Some(addresses(false).head)
          } else if (addresses.contains(true)) {
            Some(addresses(true).head)
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
      case None =>
        whoami(conf.port()) match {
          case Some(addy) => addy.getHostAddress
          case None       => "localhost"
        }
    }

    val addy = s"rnode://$name@$host:${conf.port()}"

    val net = p2p.Network(addy)
    logger.info(s"Listening for traffic on $net.")

    if (!conf.standalone()) {
      conf.bootstrap.foreach { address =>
        logger.info(s"Bootstrapping from $address.")
        net.connect(address)
      }
    } else {
      logger.info(s"Starting stand-alone node.")
    }

    sys.addShutdownHook {
      net.disconnect
      logger.info("Goodbye.")
    }

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
