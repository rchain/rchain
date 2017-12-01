package coop.rchain.comm

import org.rogach.scallop._
import java.util.UUID
import java.nio.ByteBuffer
import coop.rchain.p2p
import com.typesafe.scalalogging

case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("0.0.1 RChain communications library")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val home = opt[String](default = None, descr = "Home address for initial seed.")

  verify()
}

object Main {
  def main(args: Array[String]): Unit = {
    val logger = scalalogging.Logger("main")
    val conf = Conf(args)
    val name = UUID.randomUUID.toString.replaceAll("-", "")
    val addy = s"rnode://$name@localhost:${conf.port()}"
    val net = p2p.Network(addy)
    logger.info(s"Listening for traffic on $net.")

    conf.home.foreach { address =>
      p2p.NetworkAddress.parse(address) match {
        case Right(remote @ PeerNode(_, _)) =>
          new ProtocolNode(remote, net.net).ping
        case Left(error) => logger.error(s"Unable to bootstrap network: ${error.msg}") 
      }
    }
  }
}
