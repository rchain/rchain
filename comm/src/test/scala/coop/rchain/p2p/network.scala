package coop.rchain.p2p

import org.scalatest._
import coop.rchain.comm._
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing

class ProtocolTest extends FlatSpec with Matchers {
  "An EncryptionHandshake message" should "be answered by an EncryptionHandshakeResponse message" in {
    val uri = "rnode://abcde@localhost:12345"
    val node = NetworkAddress.parse(uri) match {
      case Right(node) => node
      case Left(_) => null
    }

    node should not be null

    val src = new ProtocolNode(node.id, node.endpoint, null)
    val hs = EncryptionHandshakeMessage(NetworkProtocol.encryptionHandshake(src), System.currentTimeMillis)
    val result = hs.response(src)

    val upstream = result match {
      case Some(message: ProtocolMessage) => message.proto.message match {
        case routing.Protocol.Message.Upstream(upstream) => upstream
        case _ => null
      }
      case _ => null
    }

    upstream should not be (null)

    val hsr = upstream.unpack(EncryptionHandshakeResponse)
    hsr should not be (null)
  }

  "A ProtocolHandshake message" should "be answered by a ProtocolHandshakeResponse message" in {
    val uri = "rnode://abcde@localhost:12345"
    val node = NetworkAddress.parse(uri) match {
      case Right(node) => node
      case Left(_) => null
    }

    node should not be null

    val src = new ProtocolNode(node.id, node.endpoint, null)
    val hs = ProtocolHandshakeMessage(NetworkProtocol.protocolHandshake(src), System.currentTimeMillis)
    val result = hs.response(src)

    val upstream = result match {
      case Some(message: ProtocolMessage) => message.proto.message match {
        case routing.Protocol.Message.Upstream(upstream) => upstream
        case _ => null
      }
      case _ => null
    }

    upstream should not be (null)

    val hsr = upstream.unpack(ProtocolHandshakeResponse)
    hsr should not be (null)
  }
}
