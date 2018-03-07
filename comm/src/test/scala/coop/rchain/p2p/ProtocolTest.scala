package coop.rchain.p2p

import org.scalatest._
import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import coop.rchain.catscontrib.ski._

/**
  * TODO rethink how the protocol is being tested
  */
class ProtocolTest extends FlatSpec with Matchers {
  "An EncryptionHandshake message" should "be answered by an EncryptionHandshakeResponse message" in {
    val uri = "rnode://abcde@localhost:12345"
    val node = NetworkAddress.parse(uri) match {
      case Right(node) => node
      case Left(_)     => null
    }

    node should not be null

    val src  = ProtocolNode(node, kp2(Left(unknownProtocol("unknown"))))
    val keys = PublicPrivateKeys(Array.empty[Byte], Array.empty[Byte])
    val hs =
      EncryptionHandshakeMessage(NetworkProtocol.encryptionHandshake(src, keys),
                                 System.currentTimeMillis)
    val result = hs.response(src, keys)

    val upstream = result match {
      case Right(message: ProtocolMessage) =>
        message.proto.message match {
          case routing.Protocol.Message.Upstream(upstream) => upstream
          case _                                           => null
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
      case Left(_)     => null
    }

    node should not be null

    val src = ProtocolNode(node, kp2(Left(unknownProtocol("unknown"))))
    val hs =
      ProtocolHandshakeMessage(NetworkProtocol.protocolHandshake(src), System.currentTimeMillis)
    val result = hs.response(src)

    val upstream = result match {
      case Right(message: ProtocolMessage) =>
        message.proto.message match {
          case routing.Protocol.Message.Upstream(upstream) => upstream
          case _                                           => null
        }
      case _ => null
    }

    upstream should not be (null)

    val hsr = upstream.unpack(ProtocolHandshakeResponse)
    hsr should not be (null)
  }
}
