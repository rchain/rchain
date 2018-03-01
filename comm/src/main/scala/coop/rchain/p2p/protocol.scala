package coop.rchain.p2p

import coop.rchain.comm._
import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

object NetworkProtocol {

  val ENCRYPTION_HELLO      = "hello"
  val ENCRYPTION_HELLO_BACK = "hello_back"

  def encryptionHandshake(src: ProtocolNode, keys: PublicPrivateKeys): routing.Protocol = {
    val msg = EncryptionHandshake(publicKey = ByteString.copyFrom(keys.pub))
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(msg))
  }

  def encryptionHandshakeResponse(src: ProtocolNode,
                                  h: routing.Header,
                                  keys: PublicPrivateKeys): routing.Protocol = {
    val msg = EncryptionHandshakeResponse(publicKey = ByteString.copyFrom(keys.pub))

    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(msg))
  }

  def toEncryptionHandshakeResponse(
      proto: routing.Protocol): Either[CommError, EncryptionHandshakeResponse] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) =>
        Right(upstream.unpack(EncryptionHandshakeResponse))
      case a => Left(UnknownProtocolError(s"Was expecting EncryptionHandshakeResponse, got $a"))
    }

  def toEncryptionHandshake(proto: routing.Protocol): Either[CommError, EncryptionHandshake] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) =>
        Right(upstream.unpack(EncryptionHandshake))
      case a => Left(UnknownProtocolError(s"Was expecting EncryptionHandshake, got $a"))
    }

  def protocolHandshake(src: ProtocolNode): routing.Protocol =
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(ProtocolHandshake()))

  def protocolHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(ProtocolHandshakeResponse()))
}

final case class EncryptionHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {

  def response(src: ProtocolNode, keys: PublicPrivateKeys): Either[CommError, ProtocolMessage] =
    for {
      h         <- header.toRight(HeaderNotAvailable)
      handshake <- NetworkProtocol.toEncryptionHandshake(proto)
      pub       = handshake.publicKey.toByteArray
    } yield {
      val message = EncryptionHandshakeResponseMessage(
        NetworkProtocol.encryptionHandshakeResponse(src, h, keys),
        System.currentTimeMillis)
      message
    }

}
final case class EncryptionHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse

final case class ProtocolHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    header.map { h =>
      ProtocolHandshakeResponseMessage(NetworkProtocol.protocolHandshakeResponse(src, h),
                                       System.currentTimeMillis)
    }
}
final case class ProtocolHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse
