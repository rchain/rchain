package coop.rchain.comm

import scala.util.Try

trait Comm {
  def send(data: Seq[Byte], p: PeerNode): Try[Unit]
  def recv: Try[Seq[Byte]]
}

case class NodeIdentifier(pKey: Seq[Byte]) {
  def keccak256(iterations: Int = 1): Seq[Byte] =
    // Not really hashing, yet.
    Vector[Byte](pKey: _*)

  def key = keccak256(2)
}

case class Endpoint(host: String, tcpPort: Int, udpPort: Int) {
  val tcpSocketAddress = new java.net.InetSocketAddress(host, tcpPort)
  val udpSocketAddress = new java.net.InetSocketAddress(host, udpPort)
}

/**
  * A PeerNode is (at least) an identifier and a network configuration.
  */
case class PeerNode(id: NodeIdentifier, endpoint: Endpoint) {

  def key = id.key

  override def toString = {
    val sKey = key.map("%02x" format _).mkString
    s"#{PeerNode $sKey}"
  }
}
