package coop.rchain.comm

import java.net.InetSocketAddress

import scala.util.control.NonFatal

import coop.rchain.crypto.codec.Base16

import io.lemonlabs.uri.{Uri, Url}

// TODO: Add Show instance
final case class NodeIdentifier(key: Seq[Byte]) {
  private val keyString         = Base16.encode(key.toArray)
  override def toString: String = keyString
}

object NodeIdentifier {
  def apply(name: String): NodeIdentifier =
    NodeIdentifier(name.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte))
}

// TODO: Add Show instance
final case class Endpoint(host: String, tcpPort: Int, udpPort: Int) {
  val tcpSocketAddress = new InetSocketAddress(host, tcpPort)
  val udpSocketAddress = new InetSocketAddress(host, udpPort)
}

// TODO: Add Show instance
final case class PeerNode(id: NodeIdentifier, endpoint: Endpoint) {

  def key: Seq[Byte] = id.key
  val sKey: String   = id.toString

  override def toString: String = toAddress

  val toAddress: String =
    s"rnode://$sKey@${endpoint.host}:${endpoint.udpPort}"

}

object PeerNode {

  final case class NetworkAddress(scheme: String, key: String, host: String, port: Int)

  def parse(str: String): Either[CommError, PeerNode] =
    // TODO replace try-catch with Try
    try {
      val url: Url = Url.parse(str)

      val addy =
        for {
          scheme <- url.schemeOption
          key    <- url.user
          host   <- url.hostOption
          port   <- url.port
        } yield NetworkAddress(scheme, key, host.value, port)

      addy match {
        case Some(NetworkAddress(_, key, host, port)) =>
          Right(PeerNode(NodeIdentifier(key), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $url"))
      }
    } catch {
      case NonFatal(_) => Left(ParseError(s"bad address: $str"))
    }
}
