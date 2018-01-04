package coop.rchain.comm

import java.net.InetSocketAddress

trait Comm[A] {
  def send(data: Seq[Byte], p: PeerNode): Either[CommError, Unit]
  def recv: Either[CommError, (A, Seq[Byte])]
}

case class NodeIdentifier(pKey: Seq[Byte]) {
  def keccak256(iterations: Int = 1): Seq[Byte] =
    // Not really hashing, yet.
    Vector[Byte](pKey: _*)

  def key = keccak256(2)

  override def toString = key.map("%02x" format _).mkString
}

case class Endpoint(host: String, tcpPort: Int, udpPort: Int) {
  val tcpSocketAddress = new InetSocketAddress(host, tcpPort)
  val udpSocketAddress = new InetSocketAddress(host, udpPort)

  def withUdp(udp: java.net.InetSocketAddress): Endpoint =
    Endpoint(udp.getHostString, tcpPort, udp.getPort)
}

/**
  * A PeerNode is (at least) an identifier and a network configuration.
  */
case class PeerNode(id: NodeIdentifier, endpoint: Endpoint) {

  def key = id.key
  val sKey = key.map(_.toChar).mkString

  override def toString =
    s"#{PeerNode $sKey}"

  def toAddress: String =
    s"rnode://$sKey@${endpoint.host}:${endpoint.udpPort}"

  def withUdpSocket(udp: java.net.InetSocketAddress): PeerNode =
    PeerNode(id, endpoint.withUdp(udp))
}

trait Notary {
  def sign(data: Seq[Byte]): Seq[Byte]
  def checkSignature(sig: Seq[Byte]): Boolean
}
