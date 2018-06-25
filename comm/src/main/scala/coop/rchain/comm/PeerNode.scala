package coop.rchain.comm

import java.net.InetSocketAddress
import com.netaporter.uri.Uri
import scala.util.control.NonFatal
import coop.rchain.crypto.codec.Base16

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

  def key  = id.key
  val sKey = id.toString

  override def toString =
    s"#{PeerNode $sKey}"

  def toAddress: String =
    s"rnode://$sKey@${endpoint.host}:${endpoint.udpPort}"

}

object PeerNode {

  final case class NetworkAddress(scheme: String, key: String, host: String, port: Int)

  def parse(str: String): Either[CommError, PeerNode] =
    // TODO replace try-catch with Try
    try {
      val uri = Uri.parse(str)

      val addy =
        for {
          scheme <- uri.scheme
          key    <- uri.user
          host   <- uri.host
          port   <- uri.port
        } yield NetworkAddress(scheme, key, host, port)

      addy match {
        case Some(NetworkAddress(_, key, host, port)) =>
          Right(PeerNode(NodeIdentifier(key), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $str"))
      }
    } catch {
      case NonFatal(_) => Left(ParseError(s"bad address: $str"))
    }
}
