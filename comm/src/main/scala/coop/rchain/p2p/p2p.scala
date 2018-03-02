package coop.rchain.p2p

import coop.rchain.comm._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import scala.util.control.NonFatal
import com.typesafe.scalalogging.Logger
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

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
        case Some(NetworkAddress(_, key, host, port)) =>
          Right(new PeerNode(NodeIdentifier(key.getBytes), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $str"))
      }
    } catch {
      case NonFatal(e) => Left(ParseError(s"bad address: $str"))
    }
}

class Network(
    local: PeerNode,
    keys: PublicPrivateKeys
) extends ProtocolDispatcher[java.net.SocketAddress] {

  import NetworkProtocol._

  val logger = Logger("p2p")
  val net    = new UnicastNetwork(local, Some(this))

  /**
    * This method (eventually) will initiate the two-part RChain handshake protocol. First, encryption keys are exchanged,
    * allowing encryption for all future messages. Next protocols are agreed on to ensure that these two nodes can speak
    * the same language.
    */
  def connect[F[_]: Capture: Monad: Log](peer: PeerNode)(
      implicit pubKeysKvs: Kvs[F, PeerNode, Array[Byte]],
      err: ApplicativeError_[F, CommError]): F[Unit] =
    for {
      _          <- Log[F].debug(s"Connecting to $peer")
      ts1        <- IOUtil.currentMilis[F]
      ehs        = EncryptionHandshakeMessage(encryptionHandshake(net.local, keys), ts1)
      remote     = new ProtocolNode(peer, this.net)
      ehsrespmsg <- net.roundTrip[F](ehs, remote)
      ehsresp    <- err.fromEither(toEncryptionHandshakeResponse(ehsrespmsg.proto))
      _          <- pubKeysKvs.put(peer, ehsresp.publicKey.toByteArray)
      _          <- Log[F].debug(s"Received encryption response from ${ehsrespmsg.sender.get}.")
      ts2        <- IOUtil.currentMilis[F]
      phs        <- ProtocolHandshakeMessage(NetworkProtocol.protocolHandshake(net.local), ts2).pure[F]
      phsresp    <- net.roundTrip[F](phs, remote)
      _          <- Log[F].debug(s"Received protocol handshake response from ${phsresp.sender.get}.")
      _          <- addNode[F](remote)
    } yield ()

  def disconnect(): Unit = {
    net.broadcast(
      DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
    ()
  }

  private def commSend[F[_]: Capture](data: Seq[Byte], peer: PeerNode): F[Either[CommError, Unit]] =
    Capture[F].capture {
      net.comm.send(data, peer)
    }

  private def addNode[F[_]: Capture](node: PeerNode): F[Unit] = Capture[F].capture {
    net.add(node)
  }

  private def handleEncryptionHandshake[F[_]: Monad: Capture: Log](
      sender: PeerNode,
      handshake: EncryptionHandshakeMessage): F[Unit] =
    for {
      result <- handshake
                 .response(net.local, keys)
                 .traverse(resp => commSend(resp.toByteSeq, sender))
      _ <- result.traverse {
            case Right(_) => Log[F].info(s"Responded to encryption handshake request from $sender.")
            case Left(ex) => Log[F].error(s"handleEncryptionHandshake(): $ex")
          }
    } yield ()

  private def handleProtocolHandshake[F[_]: Monad: Capture: Log](
      sender: PeerNode,
      handshake: ProtocolHandshakeMessage): F[Unit] =
    for {
      result <- handshake
                 .response(net.local)
                 .traverse(resp => commSend(resp.toByteSeq, sender))
      _ <- result.traverse {
            case Right(_) => Log[F].info(s"Responded to protocol handshake request from $sender.")
            case Left(ex) => Log[F].error(s"handleProtocolHandshake(): $ex")
          }
      _ <- addNode[F](sender)
    } yield ()

  override def dispatch[F[_]: Monad: Capture: Log](sock: java.net.SocketAddress,
                                                   msg: ProtocolMessage): F[Unit] = {

    val dispatchForSender: Option[F[Unit]] = msg.sender.map { sndr =>
      val sender =
        sock match {
          case (s: java.net.InetSocketAddress) =>
            sndr.withUdpSocket(s)
          case _ => sndr
        }

      msg match {
        case upstream @ UpstreamMessage(proto, _) =>
          proto.message.upstream.traverse { msg =>
            msg.typeUrl match {
              // TODO interpolate this string to check if class exists
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.EncryptionHandshake" =>
                val eh = msg.unpack(EncryptionHandshake)

                handleEncryptionHandshake[F](sender,
                                             EncryptionHandshakeMessage(proto,
                                                                        System.currentTimeMillis))
              // TODO interpolate this string to check if class exists
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.ProtocolHandshake" =>
                handleProtocolHandshake[F](sender,
                                           ProtocolHandshakeMessage(proto,
                                                                    System.currentTimeMillis))
              case _ => Log[F].warn(s"Unexpected message type ${msg.typeUrl}")
            }
          }.void
        /*
         * We do not expect to get any responses to the protocol out
         * of the blue; rather, they'll all be done through
         * synchronous, request-response messaging.
         */
        case upstream @ UpstreamResponse(proto, _) =>
          Log[F].error(s"Out-of-sequence message: $upstream")
        case _ => Log[F].warn(s"Unrecognized msg ${msg}")
      }
    }

    dispatchForSender.getOrElse(Log[F].error(s"received message with empty sender, msg = $msg"))
  }

  override def toString = s"#{Network ${local.toAddress}}"
}
