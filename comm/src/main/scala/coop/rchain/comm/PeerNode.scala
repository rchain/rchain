package coop.rchain.comm

import java.net.InetSocketAddress
import scala.util.control.NonFatal
import coop.rchain.crypto.codec.Base16
import io.lemonlabs.uri.{Uri, Url}
import scala.util.Try

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
    s"rnode://$sKey@${endpoint.host}?protocol=${endpoint.tcpPort}&discovery=${endpoint.udpPort}"

}

object PeerNode {

  import scala.util.matching.Regex, Regex._

  def fromAddress(str: String): Either[CommError, PeerNode] = {
    // TODO toInt, not URL, scheme not rnode, renameflag to discovery-port
    val maybeUrl: Option[Url] = Try(Url.parse(str)).toOption

    val maybePeer = maybeUrl flatMap (
        url =>
          for {
            scheme    <- url.schemeOption
            key       <- url.user
            host      <- url.hostOption
            discovery <- url.query.param("discovery").flatMap(v => Try(v.toInt).toOption)
            protocol  <- url.query.param("protocol").flatMap(v => Try(v.toInt).toOption)
          } yield PeerNode(NodeIdentifier(key), Endpoint(host.value, protocol, discovery))
      )

    maybePeer.fold[Either[CommError, PeerNode]](Left(ParseError(s"bad address: $str")))(Right(_))
  }
}
