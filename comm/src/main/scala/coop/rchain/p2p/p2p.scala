package coop.rchain.p2p

import coop.rchain.comm._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing
import scala.util.control.NonFatal
import com.google.protobuf.any.{Any => AnyProto}
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.Logger
import cats._, cats.data._, cats.implicits._

/*
 * Inspiration from ethereum:
 *
 *   enode://92c9722c6cd441b9d64cc3f365cdd6cda822c7496b8131078a7362b576526092b58531aa2e5914c4e87e0e574bca4497e68f35a7b66c153ee73b6c7dfc958a34@10.0.0.3:30303
 *
 * =>
 *
 *   rnode://<key>@<host>:<udp-port>
 */
final case class NetworkAddress(scheme: String, key: String, host: String, port: Int)

case object NetworkAddress {

  /**
    * Parse a string address into a [[coop.rchain.comm.PeerNode]] or
    * an error indicating that the string could not be parsed into a
    * node address.
    */
  def parse(str: String): Either[CommError, PeerNode] =
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
        case Some(NetworkAddress(scheme, key, host, port)) =>
          Right(new PeerNode(NodeIdentifier(key.getBytes), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $str"))
      }
    } catch {
      case NonFatal(e) => Left(ParseError(s"bad address: $str"))
    }
}

final case class Network(
    local: PeerNode,
    keys: PublicPrivateKeys
) extends ProtocolDispatcher[java.net.SocketAddress] {

  val logger = Logger("p2p")

  val net = new UnicastNetwork(local, Some(this))

  /**
    * This method (eventually) will initiate the two-part RChain handshake protocol. First, encryption keys are exchanged,
    * allowing encryption for all future messages. Next protocols are agreed on to ensure that these two nodes can speak
    * the same language.
    */
  def connect(peer: PeerNode): Unit = {
    logger.debug(s"connect(): Connecting to $peer")
    // EncryptionHandshakeMessage is redundant
    val ehs = EncryptionHandshakeMessage(
      NetworkProtocol.encryptionHandshake(net.local, keys),
      System.currentTimeMillis,
    )
    val remote = new ProtocolNode(peer, this.net)
    net.roundTrip(ehs, remote) match {
      case Right(resp) => {
        logger.debug(s"connect(): Received encryption handshake response from ${resp.sender.get}.")
        val phs = ProtocolHandshakeMessage(NetworkProtocol.protocolHandshake(net.local),
                                           System.currentTimeMillis)
        net.roundTrip(phs, remote) match {
          case Right(resp) => {
            logger.debug(
              s"connect(): Received protocol handshake response from ${resp.sender.get}.")
            net.add(remote)
          }
          case Left(ex) => logger.warn(s"connect(): No phs response: $ex")
        }
      }
      case Left(ex) => logger.warn(s"connect(): No ehs response: $ex")
    }
  }

  def disconnect(): Unit = {
    net.broadcast(
      DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
    ()
  }

  // TODO move response to this function and remove repakaging to EncryptionHandshakeMessage
  private def handleEncryptionHandshake(sender: PeerNode,
                                        handshake: EncryptionHandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      net.comm.send(resp.toByteSeq, sender) match {
        case Right(_) =>
          logger.info(s"Responded to encryption handshake request from $sender.")
        case Left(ex) =>
          logger.error(s"handleEncryptionHandshake(): $ex")
      }
    }

  private def handleProtocolHandshake(sender: PeerNode, handshake: ProtocolHandshakeMessage): Unit =
    for {
      resp <- handshake.response(net.local)
    } {
      net.comm.send(resp.toByteSeq, sender) match {
        case Right(_) =>
          logger.info(s"Responded to protocol handshake request from $sender.")
        case Left(ex) =>
          logger.error(s"handleProtocolHandshake(): $ex")
      }
      // TODO: This add() call should be conditional on actually going
      // through the handshake process, but for now it helps
      // demonstrate network-building.
      net.add(sender)
    }

  override def dispatch(sock: java.net.SocketAddress, msg: ProtocolMessage): Unit =
    for {
      sndr <- msg.sender
    } {
      val sender =
        sock match {
          case (s: java.net.InetSocketAddress) =>
            sndr.withUdpSocket(s)
          case _ => sndr
        }

      msg match {
        case upstream @ UpstreamMessage(proto, _) =>
          proto.message.upstream.map { msg =>
            msg.typeUrl match {
              // TODO interpolate this string to check if class exists
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.EncryptionHandshake" =>
                val eh = msg.unpack(EncryptionHandshake)

                handleEncryptionHandshake(
                  sender,
                  EncryptionHandshakeMessage(proto, System.currentTimeMillis)) //FIX-ME
              // TODO interpolate this string to check if class exists
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.ProtocolHandshake" =>
                handleProtocolHandshake(sender,
                                        ProtocolHandshakeMessage(proto, System.currentTimeMillis))
              case _ => logger.warn(s"Unexpected message type ${msg.typeUrl}")
            }
          }
        /*
         * We do not expect to get any responses to the protocol out
         * of the blue; rather, they'll all be done through
         * synchronous, request-response messaging.
         */
        case upstream @ UpstreamResponse(proto, _) =>
          logger.error(s"Out-of-sequence message: $upstream")
        case _ => logger.warn(s"Unrecognized msg ${msg}")
      }
    }

  override def toString = s"#{Network ${local.toAddress}}"
}

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
