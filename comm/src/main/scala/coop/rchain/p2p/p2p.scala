package coop.rchain.p2p

import coop.rchain.comm._, CommError._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import scala.util.control.NonFatal
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

object Network extends ProtocolDispatcher[java.net.SocketAddress] {

  import NetworkProtocol._

  def unsafeRoundTrip[F[_]: Capture: Communication]
    : (ProtocolMessage, ProtocolNode) => CommErr[ProtocolMessage] =
    (pm: ProtocolMessage, pn: ProtocolNode) => {
      val result = Communication[F].roundTrip(pm, pn)
      Capture[F].unsafeUncapture(result)
    }

  def connect[F[_]: Capture: Monad: Log: Time: Metrics: Communication: Encryption](peer: PeerNode)(
      implicit
      keysStore: Kvs[F, PeerNode, Array[Byte]],
      err: ApplicativeError_[F, CommError]): F[Unit] =
    for {
      keys       <- Encryption[F].fetchKeys
      _          <- Log[F].debug(s"Connecting to $peer")
      _          <- Metrics[F].incrementCounter("connects")
      ts1        <- Time[F].currentMillis
      local      <- Communication[F].local
      ehs        = EncryptionHandshakeMessage(encryptionHandshake(local, keys), ts1)
      remote     = ProtocolNode(peer, local, unsafeRoundTrip[F])
      ehsrespmsg <- Communication[F].roundTrip(ehs, remote).map(err.fromEither).flatten
      ehsresp    <- err.fromEither(toEncryptionHandshakeResponse(ehsrespmsg.proto))
      _          <- keysStore.put(peer, ehsresp.publicKey.toByteArray)
      _          <- Log[F].debug(s"Received encryption response from ${ehsrespmsg.sender.get}.")
      ts2        <- Time[F].currentMillis
      phs        <- ProtocolHandshakeMessage(NetworkProtocol.protocolHandshake(local), ts2).pure[F]
      phsresp    <- Communication[F].roundTrip(phs, remote).map(err.fromEither).flatten
      _          <- Log[F].debug(s"Received protocol handshake response from ${phsresp.sender.get}.")
      _          <- Communication[F].addNode(remote)
      tsf        <- Time[F].currentMillis
      _          <- Metrics[F].record("connect-time-ms", tsf - ts1)
    } yield ()

  private def handleEncryptionHandshake[
      F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption](
      sender: PeerNode,
      handshake: EncryptionHandshakeMessage)(
      implicit keysStore: Kvs[F, PeerNode, Array[Byte]]): F[Unit] =
    for {
      _           <- Metrics[F].incrementCounter("p2p-encryption-handshake-recv-count")
      local       <- Communication[F].local
      keys        <- Encryption[F].fetchKeys
      responseErr <- handshake.response[F](local, keys)
      result      <- responseErr.traverse(resp => Communication[F].commSend(resp.toByteSeq, sender))
      _ <- result.traverse {
            case Right(_) => Log[F].info(s"Responded to encryption handshake request from $sender.")
            case Left(ex) => Log[F].error(s"handleEncryptionHandshake(): $ex")
          }
    } yield ()

  private def handleProtocolHandshake[F[_]: Monad: Capture: Log: Metrics: Communication](
      sender: PeerNode,
      handshake: ProtocolHandshakeMessage): F[Unit] =
    for {
      _     <- Metrics[F].incrementCounter("p2p-protocol-handshake-recv-count")
      local <- Communication[F].local
      result <- handshake
                 .response(local)
                 .traverse(resp => Communication[F].commSend(resp.toByteSeq, sender))
      _ <- result.traverse {
            case Right(_) => Log[F].info(s"Responded to protocol handshake request from $sender.")
            case Left(ex) => Log[F].error(s"handleProtocolHandshake(): $ex")
          }
      _ <- Communication[F].addNode(sender)
    } yield ()

  override def dispatch[
      F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption: Kvs[?[_],
                                                                               PeerNode,
                                                                               Array[Byte]]](
      sock: java.net.SocketAddress,
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
}
