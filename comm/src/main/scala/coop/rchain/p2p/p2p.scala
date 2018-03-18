package coop.rchain.p2p

import scala.concurrent.duration.{Duration, MILLISECONDS}
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.routing, routing.Header
import coop.rchain.comm._, CommError._
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import scala.util.control.NonFatal
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._

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
  import Encryption._

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
      keys         <- Encryption[F].fetchKeys
      _            <- Log[F].debug(s"Connecting to $peer")
      _            <- Metrics[F].incrementCounter("connects")
      ts1          <- Time[F].currentMillis
      local        <- Communication[F].local
      ehs          = EncryptionHandshakeMessage(encryptionHandshake(local, keys), ts1)
      remote       = ProtocolNode(peer, local, unsafeRoundTrip[F])
      ehsrespmsg   <- Communication[F].roundTrip(ehs, remote) >>= (err.fromEither _)
      ehsresp      <- err.fromEither(toEncryptionHandshakeResponse(ehsrespmsg.proto))
      remotePubKey = ehsresp.publicKey.toByteArray
      _            <- keysStore.put(peer, remotePubKey)
      _            <- Log[F].debug(s"Received encryption response from ${ehsrespmsg.sender.get}.")
      fm           <- frameMessage[F](remote, nonce => protocolHandshake(local, nonce))
      phsresp      <- Communication[F].roundTrip(fm, remote) >>= err.fromEither
      _            <- Log[F].debug(s"Received protocol handshake response from ${phsresp.sender.get}.")
      _            <- Communication[F].addNode(remote)
      tsf          <- Time[F].currentMillis
      _            <- Metrics[F].record("connect-time-ms", tsf - ts1)
    } yield ()

  def handleEncryptionHandshake[
      F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption](
      sender: PeerNode,
      msg: EncryptionHandshakeMessage)(implicit keysStore: Kvs[F, PeerNode, Array[Byte]]): F[Unit] =
    for {
      _            <- Metrics[F].incrementCounter("p2p-encryption-handshake-recv-count")
      local        <- Communication[F].local
      keys         <- Encryption[F].fetchKeys
      handshakeErr <- NetworkProtocol.toEncryptionHandshake(msg.proto).pure[F]
      _ <- handshakeErr.fold(kp(Log[F].error("could not fetch proto message")),
                             hs => keysStore.put(sender, hs.publicKey.toByteArray))
      responseErr <- msg.response[F](local, keys)
      result      <- responseErr.traverse(resp => Communication[F].commSend(resp, sender))
      _ <- result.traverse {
            case Right(_) => Log[F].info(s"Responded to encryption handshake request from $sender.")
            case Left(ex) => Log[F].error(s"handleEncryptionHandshake(): $ex")
          }
    } yield ()

  def handleFrame[F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption](
      remote: PeerNode,
      msg: FrameMessage)(implicit
                         err: ApplicativeError_[F, CommError],
                         keysStore: Kvs[F, PeerNode, Array[Byte]]): F[String] =
    for {
      _                 <- Metrics[F].incrementCounter("p2p-protocol-handshake-recv-count")
      maybeRemotePubKey <- keysStore.get(remote)
      keys              <- Encryption[F].fetchKeys
      remotePubKey <- maybeRemotePubKey
                       .map(_.pure[F])
                       .getOrElse(err.raiseError(peerNodeNotFound(remote)))
      frame          <- err.fromEither(NetworkProtocol.toFrame(msg.proto))
      nonce          = frame.nonce.toByteArray
      encryptedBytes = frame.framed.toByteArray
      decryptedBytes <- Encryption[F].decrypt(remotePubKey, keys.priv, nonce, encryptedBytes)
      unframed       = Frameable.parseFrom(decryptedBytes).message
      res <- if (unframed.isProtocolHandshake) {
              handleProtocolHandshake[F](remote, msg.header, unframed.protocolHandshake)
            } else
              err.fromEither(
                Left(unknownProtocol(s"Received unhandable message in frame: $unframed")))
    } yield res

  private def handleProtocolHandshake[F[_]: Monad: Time: Communication: Encryption](
      remote: PeerNode,
      maybeHeader: Option[Header],
      maybePh: Option[ProtocolHandshake])(implicit
                                          keysStore: Kvs[F, PeerNode, Array[Byte]],
                                          err: ApplicativeError_[F, CommError]): F[String] =
    for {
      local  <- Communication[F].local
      ph     <- getOrError[F, ProtocolHandshake](maybePh, parseError("ProtocolHandshake"))
      h      <- getOrError[F, Header](maybeHeader, headerNotAvailable)
      fm     <- frameResponseMessage[F](remote, h, nonce => protocolHandshakeResponse(local, nonce))
      result <- Communication[F].commSend(fm, remote)
      _      <- Communication[F].addNode(remote)
    } yield s"Responded to protocol handshake request from $remote"

  override def dispatch[F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption: Kvs[
    ?[_],
    PeerNode,
    Array[Byte]]: ApplicativeError_[?[_], CommError]](sock: java.net.SocketAddress,
                                                      msg: ProtocolMessage): F[Unit] = {

    val dispatchForSender: Option[F[Unit]] = msg.sender.map { sndr =>
      val sender =
        sock match {
          case (s: java.net.InetSocketAddress) => sndr.withUdpSocket(s)
          case _                               => sndr
        }

      msg match {
        case upstream @ UpstreamMessage(proto, _) =>
          proto.message.upstream.traverse { msg =>
            msg.typeUrl match {
              // TODO interpolate this string to check if class exists
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.EncryptionHandshake" =>
                handleEncryptionHandshake[F](sender,
                                             EncryptionHandshakeMessage(proto,
                                                                        System.currentTimeMillis))
              // TODO interpolate this string to check if class exists
              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Frame" =>
                val err     = ApplicativeError_[F, CommError]
                val handled = handleFrame[F](sender, FrameMessage(proto, System.currentTimeMillis))
                err.attempt(handled) >>= {
                  case Right(res) => Log[F].info(res)
                  case Left(err)  => Log[F].error(s"error while handling frame message: $err")
                }
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

  private def getOrError[F[_]: Applicative, A](oa: Option[A], error: CommError)(
      implicit
      err: ApplicativeError_[F, CommError]): F[A] =
    oa.fold[F[A]](err.raiseError[A](error))(_.pure[F])

  private def frameMessage[F[_]: Monad: Time: Communication: Encryption](
      remote: PeerNode,
      frameable: Nonce => Frameable)(implicit keysStore: Kvs[F, PeerNode, Array[Byte]],
                                     err: ApplicativeError_[F, CommError]): F[FrameMessage] =
    frameIt[F](remote, frameable, (local, nonce, f) => frame(local, nonce, f))

  private def frameResponseMessage[F[_]: Monad: Time: Communication: Encryption](
      remote: PeerNode,
      header: Header,
      frameable: Nonce => Frameable)(implicit keysStore: Kvs[F, PeerNode, Array[Byte]],
                                     err: ApplicativeError_[F, CommError]): F[FrameMessage] =
    frameIt[F](remote, frameable, (local, nonce, f) => frameResponse(local, header, nonce, f))

  private def frameIt[F[_]: Monad: Time: Communication: Encryption](
      remote: PeerNode,
      frameable: Nonce => Frameable,
      proto: (ProtocolNode, Nonce, Array[Byte]) => routing.Protocol)(
      implicit keysStore: Kvs[F, PeerNode, Array[Byte]],
      err: ApplicativeError_[F, CommError]): F[FrameMessage] =
    for {
      local        <- Communication[F].local
      nonce        <- Encryption[F].generateNonce
      keys         <- Encryption[F].fetchKeys
      remotePubKey <- fetchRemotePublicKey[F](remote)
      framed       = frameable(nonce).toByteArray
      f            <- Encryption[F].encrypt(pub = remotePubKey, sec = keys.priv, nonce, framed)
      ts           <- Time[F].currentMillis
    } yield FrameMessage(proto(local, nonce, f), ts)

  private def fetchRemotePublicKey[F[_]: Monad](remote: PeerNode)(
      implicit keysStore: Kvs[F, PeerNode, Array[Byte]],
      err: ApplicativeError_[F, CommError]): F[Key] =
    for {
      maybeKey <- keysStore.get(remote)
      key      <- err.fromEither(maybeKey.toRight(publicKeyNotAvailable(remote)))
    } yield key
}
