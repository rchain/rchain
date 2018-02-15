package coop.rchain.p2p

import coop.rchain.comm._
import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import cats._, cats.data._, cats.implicits._

object NetworkProtocol {

  val ENCRYPTION_HELLO = "hello"

  def encryptionHandshake(src: ProtocolNode, keys: PublicPrivateKeys): routing.Protocol = {
    val signIt: Array[Byte] => Array[Byte] = encryption.sign(keys.priv, _)
    val prepare: String => Array[Byte]     = encryption.hashIt >>> signIt
    val msg = EncryptionHandshake(publicKey = ByteString.copyFrom(keys.pub),
                                  hello = ByteString.copyFrom(prepare(ENCRYPTION_HELLO)))
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(msg))
  }

  def encryptionHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(EncryptionHandshakeResponse()))

  def protocolHandshake(src: ProtocolNode): routing.Protocol =
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(ProtocolHandshake()))

  def protocolHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(ProtocolHandshakeResponse()))
}

final case class EncryptionHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    header.map { h =>
      EncryptionHandshakeResponseMessage(NetworkProtocol.encryptionHandshakeResponse(src, h),
                                         System.currentTimeMillis)
    }
}
final case class EncryptionHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse

final case class ProtocolHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield
      ProtocolHandshakeResponseMessage(NetworkProtocol.protocolHandshakeResponse(src, h),
                                       System.currentTimeMillis)
}
final case class ProtocolHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolResponse
