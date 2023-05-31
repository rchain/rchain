package coop.rchain.comm

import coop.rchain.comm.protocol.routing.{Node, Packet}
import coop.rchain.shared.Base16
import io.lemonlabs.uri.Url

import scala.util.Try

// TODO: Add Show instance
final case class NodeIdentifier(key: Seq[Byte]) {
  private val keyString         = Base16.encode(key.toArray)
  override def toString: String = keyString
}

object NodeIdentifier {
  def apply(name: String): NodeIdentifier =
    NodeIdentifier(name.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte).toIndexedSeq)
}

// TODO: Add Show instance
final case class Endpoint(host: String, tcpPort: Int, udpPort: Int)

// TODO: Add Show instance
final case class PeerNode(id: NodeIdentifier, endpoint: Endpoint) {

  def key: Seq[Byte] = id.key
  val sKey: String   = id.toString

  override def toString: String = toAddress

  val toAddress: String =
    s"rnode://$sKey@${endpoint.host}?protocol=${endpoint.tcpPort}&discovery=${endpoint.udpPort}"

}

final case class RoutingMessage(peer: PeerNode, packet: Packet)

object PeerNode {

  def from(id: NodeIdentifier, host: String, protocol: Int, discovery: Int): PeerNode =
    PeerNode(id, Endpoint(host, protocol, discovery))

  def from(node: Node): PeerNode =
    PeerNode(
      NodeIdentifier(node.id.toByteArray.toIndexedSeq),
      Endpoint(node.host.toStringUtf8, node.tcpPort, node.udpPort)
    )

  def fromAddress(str: String): Either[CommError, PeerNode] = {
    // TODO toInt, not URL, scheme not rnode, renameflag to discovery-port
    val maybeUrl: Option[Url] = Try(Url.parse(str)).toOption

    val maybePeer = maybeUrl flatMap (
        url =>
          for {
            _         <- url.schemeOption
            id        <- url.user
            host      <- url.hostOption
            discovery <- url.query.param("discovery").flatMap(v => Try(v.toInt).toOption)
            protocol  <- url.query.param("protocol").flatMap(v => Try(v.toInt).toOption)
          } yield from(NodeIdentifier(id), host.value, protocol, discovery)
      )

    maybePeer.fold[Either[CommError, PeerNode]](Left(ParseError(s"bad address: $str")))(Right(_))
  }
}
