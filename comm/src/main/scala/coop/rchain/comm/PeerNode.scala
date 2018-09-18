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
final case class Endpoint(host: String, tcpPort: Int, udpPort: Int)

// TODO: Add Show instance
final case class PeerNode(id: NodeIdentifier, endpoint: Endpoint) {

  def key: Seq[Byte] = id.key
  val sKey: String   = id.toString

  override def toString: String = toAddress

  val toAddress: String =
    s"rnode://$sKey@${endpoint.host}:[${endpoint.tcpPort},${endpoint.udpPort}]"

}

object PeerNode {

  import scala.util.matching.Regex, Regex._

  def fromAddress(str: String): Either[CommError, PeerNode] = {
    val template = """rnode\:\/\/(.*?)@(.*?):\[(.*),(.*)\]""".r

    str match {
      case template(key, host, tcpPort, udpPort) =>
        Right(PeerNode(NodeIdentifier(key), Endpoint(host, tcpPort.toInt, udpPort.toInt)))
      case _ => Left(ParseError(s"bad address: $str"))
    }
  }
}
