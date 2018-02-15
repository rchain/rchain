package coop.rchain.p2p

import coop.rchain.comm._
import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.instances._

object NetworkProtocol {

  val ENCRYPTION_HELLO      = "hello"
  val ENCRYPTION_HELLO_BACK = "hello_back"

  def encryptionHandshake(src: ProtocolNode, keys: PublicPrivateKeys): routing.Protocol = {
    val signIt: Array[Byte] => Array[Byte] = encryption.sign(keys.priv, _)
    val prepare: String => Array[Byte]     = encryption.hashIt >>> signIt
    val msg = EncryptionHandshake(publicKey = ByteString.copyFrom(keys.pub),
                                  hello = ByteString.copyFrom(prepare(ENCRYPTION_HELLO)))
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(msg))
  }

  def encryptionHandshakeResponse(src: ProtocolNode,
                                  h: routing.Header,
                                  keys: PublicPrivateKeys): routing.Protocol = {
    val signIt: Array[Byte] => Array[Byte] = encryption.sign(keys.priv, _)
    val prepare: String => Array[Byte]     = encryption.hashIt >>> signIt
    val msg = EncryptionHandshakeResponse(publicKey = ByteString.copyFrom(keys.pub),
                                          helloBack =
                                            ByteString.copyFrom(prepare(ENCRYPTION_HELLO_BACK)))

    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(msg))
  }

  def protocolHandshake(src: ProtocolNode): routing.Protocol =
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(ProtocolHandshake()))

  def protocolHandshakeResponse(src: ProtocolNode, h: routing.Header): routing.Protocol =
    ProtocolMessage.upstreamResponse(src, h, AnyProto.pack(ProtocolHandshakeResponse()))
}

final case class EncryptionHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {

  private def toEncryptionHandshake(
      proto: routing.Protocol): Either[CommError, EncryptionHandshake] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) =>
        Right(upstream.unpack(EncryptionHandshake))
      case a => Left(UnknownProtocolError(s"Was expecting EncryptionHandshake, got $a"))
    }

  def response(src: ProtocolNode, keys: PublicPrivateKeys): Either[CommError, ProtocolMessage] =
    for {
      h         <- header.toRight(HeaderNotAvailable)
      handshake <- toEncryptionHandshake(proto)
      pub       = handshake.publicKey.toByteArray
      signature = handshake.hello.toByteArray
      hash      = encryption.hashIt(NetworkProtocol.ENCRYPTION_HELLO)
      verified  = encryption.verify(pub, signature, hash)
      _ <- verified.either(()).or(EncryptionHandshakeIncorrectlySigned)
    } yield
      EncryptionHandshakeResponseMessage(NetworkProtocol.encryptionHandshakeResponse(src, h, keys),
                                         System.currentTimeMillis)
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
