package coop.rchain.comm

import org.rogach.scallop._
import java.util.UUID
import java.nio.ByteBuffer
import coop.rchain.p2p
import com.typesafe.scalalogging.Logger

case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("0.0.1 RChain communications library")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val home = opt[String](default = None, descr = "Home address for initial seed.")

  verify()
}

object Main {
  def main(args: Array[String]): Unit = {
    val logger = Logger("main")
    val conf = Conf(args)
    val name = UUID.randomUUID.toString.replaceAll("-", "")
    val addy = s"rnode://$name@localhost:${conf.port()}"
    val net = p2p.Network(addy)
    logger.info(s"Listening for traffic on $net.")

    conf.home.foreach { address =>
      logger.info(s"Bootstrapping from $address.")
      net.connect(address)
    }

    sys.addShutdownHook {
      net.disconnect
      logger.info("Goodbye.")
    }

    var lastCount = 0
    while (true) {
      Thread.sleep(5000)
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
