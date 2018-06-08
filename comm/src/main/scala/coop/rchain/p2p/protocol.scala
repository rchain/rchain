package coop.rchain.p2p

import coop.rchain.comm._, CommError._
import com.google.protobuf.ByteString
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import coop.rchain.p2p.effects._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, Encryption.{Key, Nonce}

object NetworkProtocol {

  val ENCRYPTION_HELLO      = "hello"
  val ENCRYPTION_HELLO_BACK = "hello_back"

  def encryptionHandshake(src: PeerNode, keys: PublicPrivateKeys): routing.Protocol = {
    val msg = EncryptionHandshake(publicKey = ByteString.copyFrom(keys.pub))
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(msg))
  }

  def frame(src: PeerNode, nonce: Nonce, framed: Array[Byte]): routing.Protocol = {
    val msg = Frame(ByteString.copyFrom(nonce), ByteString.copyFrom(framed))
    ProtocolMessage.upstreamMessage(src, AnyProto.pack(msg))
  }

  def frameResponse(src: PeerNode,
                    h: routing.Header,
                    nonce: Nonce,
                    framed: Array[Byte]): routing.Protocol = {
    val msg = Frame(ByteString.copyFrom(nonce), ByteString.copyFrom(framed))
    ProtocolMessage.upstreamResponse(src, AnyProto.pack(msg))
  }

  def encryptionHandshakeResponse(src: PeerNode,
                                  h: routing.Header,
                                  keys: PublicPrivateKeys): routing.Protocol = {
    val msg = EncryptionHandshakeResponse(publicKey = ByteString.copyFrom(keys.pub))

    ProtocolMessage.upstreamResponse(src, AnyProto.pack(msg))
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

  def toFrame(proto: routing.Protocol): Either[CommError, Frame] =
    proto.message match {
      case routing.Protocol.Message.Upstream(upstream) =>
        Right(upstream.unpack(Frame))
      case a => Left(UnknownProtocolError(s"Was expecting Frame, got $a"))
    }

  def protocolHandshake(src: PeerNode, nonce: Nonce): Frameable =
    Frameable(Frameable.Message.ProtocolHandshake(ProtocolHandshake(ByteString.copyFrom(nonce))))

  def protocolHandshakeResponse(src: PeerNode, nonce: Nonce): Frameable =
    Frameable(
      Frameable.Message.ProtocolHandshakeResponse(
        ProtocolHandshakeResponse(ByteString.copyFrom(nonce))))

  def framePacket(src: PeerNode, content: ByteString): Frameable =
    framePacket(src, Packet(content))

  def framePacket(src: PeerNode, packet: Packet): Frameable =
    Frameable(Frameable.Message.Packet(packet))
}

final case class EncryptionHandshakeMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage {

  def response[F[_]: Monad: Time](src: PeerNode,
                                  keys: PublicPrivateKeys): F[Either[CommError, ProtocolMessage]] =
    for {
      ts   <- Time[F].currentMillis
      hErr <- header.toRight(headerNotAvailable).pure[F]
    } yield {
      hErr.map { h =>
        val p = NetworkProtocol.encryptionHandshakeResponse(src, h, keys)
        EncryptionHandshakeResponseMessage(p, ts)
      }
    }
}

final case class EncryptionHandshakeResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage

final case class FrameMessage(proto: routing.Protocol, timestamp: Long) extends ProtocolMessage

final case class FrameResponseMessage(proto: routing.Protocol, timestamp: Long)
    extends ProtocolMessage
