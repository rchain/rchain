package coop.rchain.comm

import org.rogach.scallop._
import java.util.UUID
import java.nio.ByteBuffer
import coop.rchain.p2p

case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("0.0.1 RChain communications library")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val home = opt[String](default = None, descr = "Home address for initial seed.")

  verify()
}

object Main {
  def main(args: Array[String]): Unit = {
    val conf = Conf(args)
    val name = UUID.randomUUID.toString.replaceAll("-", "")
    val addy = s"rnode://$name@localhost:${conf.port()}"
    val net = p2p.Network(addy)
    println(s"Listening for traffic on $net.")

    for {
      home <- conf.home
      remoteAddress <- p2p.NetworkAddress.parse(home)
    } remoteAddress match {
      case remote@PeerNode(_, _) => {
        new ProtocolNode(remote, net.net).ping
      }
      case _ => println(s"Bad home node address: $home.")
    }
  }
}
