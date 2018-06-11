package coop.rchain.p2p

import coop.rchain.p2p.effects._

import scala.concurrent.duration.{Duration, MILLISECONDS}
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.routing, routing.Header
import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing.{Protocol => RoutingProtocol}
import com.netaporter.uri.Uri
import coop.rchain.comm.protocol.rchain._
import coop.rchain.metrics.Metrics

import scala.util.control.NonFatal
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import com.google.protobuf.ByteString
import CommunicationResponse._
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
          Right(PeerNode(NodeIdentifier(key), Endpoint(host, port, port)))
        case _ => Left(ParseError(s"bad address: $str"))
      }
    } catch {
      case NonFatal(_) => Left(ParseError(s"bad address: $str"))
    }
}

object Network {

  type KeysStore[F[_]]    = Kvs[F, PeerNode, Array[Byte]]
  type ErrorHandler[F[_]] = ApplicativeError_[F, CommError]

  import NetworkProtocol._
  import Encryption._

  val defaultTimeout: Duration = Duration(500, MILLISECONDS)

  def findAndConnect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: Encryption: KeysStore: ErrorHandler]
    : Int => F[Int] =
    (lastCount: Int) =>
      for {
        _         <- IOUtil.sleep[F](5000L)
        peers     <- NodeDiscovery[F].findMorePeers(10)
        _         <- peers.toList.traverse(connect[F](_, defaultTimeout).attempt)
        thisCount <- NodeDiscovery[F].peers.map(_.size)
        _         <- (thisCount != lastCount).fold(Log[F].info(s"Peers: $thisCount."), ().pure[F])
      } yield thisCount

  def connectToBootstrap[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: Encryption: KeysStore: ErrorHandler](
      bootstrapAddrStr: String,
      maxNumOfAttempts: Int = 5): F[Unit] = {

    def connectAttempt(attempt: Int, timeout: Duration, bootstrapAddr: PeerNode): F[Unit] =
      if (attempt > maxNumOfAttempts) for {
        _ <- Log[F].error("Failed to connect to bootstrap node, exiting...")
        _ <- errorHandler[F].raiseError[Unit](couldNotConnectToBootstrap)
      } yield ()
      else
        for {
          res <- connect[F](bootstrapAddr, timeout).attempt
          _ <- res match {
                case Left(_) =>
                  val msg = s"Failed to connect to bootstrap (attempt $attempt / $maxNumOfAttempts)"
                  Log[F].warn(msg) *> connectAttempt(attempt + 1,
                                                     timeout + defaultTimeout,
                                                     bootstrapAddr)
                case Right(_) => ().pure[F]
              }
        } yield ()

    for {
      bootstrapAddr <- errorHandler[F].fromEither(NetworkAddress.parse(bootstrapAddrStr))
      _             <- Log[F].info(s"Bootstrapping from $bootstrapAddr.")
      _             <- connectAttempt(attempt = 1, defaultTimeout, bootstrapAddr)
      _             <- Log[F].info(s"Connected $bootstrapAddr.")
    } yield ()
  }

  def connect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: Encryption: KeysStore: ErrorHandler](
      peer: PeerNode,
      timeout: Duration): F[Unit] = {

    def firstPhase: F[PeerNode] =
      for {
        _            <- Log[F].info(s"Initialize first phase handshake (encryption handshake) to $peer")
        keys         <- Encryption[F].fetchKeys
        ts1          <- Time[F].currentMillis
        local        <- TransportLayer[F].local
        ehs          = EncryptionHandshakeMessage(encryptionHandshake(local, keys), ts1)
        ehsrespmsg   <- TransportLayer[F].roundTrip(ehs, peer, timeout) >>= (errorHandler[F].fromEither _)
        ehsresp      <- errorHandler[F].fromEither(toEncryptionHandshakeResponse(ehsrespmsg.proto))
        remotePubKey = ehsresp.publicKey.toByteArray
        _            <- keysStore[F].put(peer, remotePubKey)
        _            <- Log[F].debug(s"Received encryption response from ${ehsrespmsg.sender.get}.")
      } yield peer

    def secondPhase: F[Unit] =
      for {
        _       <- Log[F].info(s"Initialize second phase handshake (protocol handshake) to $peer")
        local   <- TransportLayer[F].local
        fm      <- frameMessage[F](peer, nonce => protocolHandshake(local, nonce))
        phsresp <- TransportLayer[F].roundTrip(fm, peer, timeout) >>= errorHandler[F].fromEither
        _       <- Log[F].debug(s"Received protocol handshake response from ${phsresp.sender.get}.")
        _       <- NodeDiscovery[F].addNode(peer)
      } yield ()

    def fullHandshake: F[Unit] = firstPhase *> secondPhase

    def secondPhaseOrFull: F[Unit] =
      for {
        res <- secondPhase.attempt
        _   <- res.fold(kp(fullHandshake), _.pure[F])
      } yield ()

    for {
      tss      <- Time[F].currentMillis
      _        <- Log[F].debug(s"Connecting to $peer")
      _        <- Metrics[F].incrementCounter("connects")
      maybeKey <- keysStore[F].get(peer)
      _        <- maybeKey.fold(fullHandshake)(kp(secondPhaseOrFull))
      tsf      <- Time[F].currentMillis
      _        <- Metrics[F].record("connect-time-ms", tsf - tss)
    } yield ()
  }

  def handleEncryptionHandshake[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: Encryption: ErrorHandler: KeysStore](
      sender: PeerNode,
      msg: EncryptionHandshakeMessage): F[CommunicationResponse] =
    for {
      _            <- Metrics[F].incrementCounter("p2p-encryption-handshake-recv-count")
      local        <- TransportLayer[F].local
      keys         <- Encryption[F].fetchKeys
      handshakeErr <- NetworkProtocol.toEncryptionHandshake(msg.proto).pure[F]
      _ <- handshakeErr.fold(kp(Log[F].error("could not fetch proto message")),
                             hs => keysStore[F].put(sender, hs.publicKey.toByteArray))
      responseErr <- msg.response[F](local, keys)
      response    <- errorHandler[F].fromEither(responseErr)
      _           <- Log[F].info(s"Responded to encryption handshake request from $sender.")
    } yield handledWithMessage(response)

  def handlePacket[
      F[_]: Monad: Time: TransportLayer: Encryption: KeysStore: ErrorHandler: Log: PacketHandler](
      remote: PeerNode,
      maybePacket: Option[Packet]): F[CommunicationResponse] = {
    val errorMsg = s"Expecting Packet from frame, got something else. Stopping the node."
    def handleNone: F[CommunicationResponse] =
      for {
        _ <- Log[F].error(errorMsg)
        _ <- errorHandler[F].raiseError[Unit](unknownCommError(errorMsg))
      } yield notHandled

    maybePacket.fold(handleNone)(p =>
      for {
        maybeResponsePacket <- PacketHandler[F].handlePacket(remote, p)
        maybeResponsePacketMessage <- maybeResponsePacket.traverse(rp =>
                                       frameMessage[F](remote, kp(framePacket(remote, rp))))
      } yield maybeResponsePacketMessage.fold(notHandled)(m => handledWithMessage(m)))
  }

  def handleFrame[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: NodeDiscovery: Encryption: PacketHandler: ErrorHandler: KeysStore](
      remote: PeerNode,
      msg: FrameMessage): F[CommunicationResponse] =
    for {
      _                 <- Metrics[F].incrementCounter("p2p-protocol-handshake-recv-count")
      maybeRemotePubKey <- keysStore[F].get(remote)
      keys              <- Encryption[F].fetchKeys
      remotePubKey <- maybeRemotePubKey
                       .map(_.pure[F])
                       .getOrElse(errorHandler[F].raiseError(peerNodeNotFound(remote)))
      frame          <- errorHandler[F].fromEither(NetworkProtocol.toFrame(msg.proto))
      nonce          = frame.nonce.toByteArray
      encryptedBytes = frame.framed.toByteArray
      decryptedBytes <- Encryption[F].decrypt(remotePubKey, keys.priv, nonce, encryptedBytes)
      unframed       = Frameable.parseFrom(decryptedBytes).message
      res <- if (unframed.isProtocolHandshake) {
              handleProtocolHandshake[F](remote, msg.header, unframed.protocolHandshake)
            } else if (unframed.isPacket) {
              handlePacket[F](remote, unframed.packet)
            } else
              errorHandler[F]
                .fromEither(
                  Left(unknownProtocol(s"Received unhandable message in frame: $unframed")))
                .as(notHandled)
    } yield res

  private def handleProtocolHandshake[
      F[_]: Monad: Time: TransportLayer: NodeDiscovery: Encryption: Log: ErrorHandler: KeysStore](
      remote: PeerNode,
      maybeHeader: Option[Header],
      maybePh: Option[ProtocolHandshake]): F[CommunicationResponse] =
    for {
      local <- TransportLayer[F].local
      _     <- getOrError[F, ProtocolHandshake](maybePh, parseError("ProtocolHandshake"))
      h     <- getOrError[F, Header](maybeHeader, headerNotAvailable)
      fm    <- frameResponseMessage[F](remote, h, nonce => protocolHandshakeResponse(local, nonce))
      _     <- NodeDiscovery[F].addNode(remote)
      _     <- Log[F].info(s"Responded to protocol handshake request from $remote")
    } yield handledWithMessage(fm)

  // TODO F is tooooo rich
  def dispatch[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: NodeDiscovery: Encryption: KeysStore: ErrorHandler: PacketHandler](
      msg: ProtocolMessage): F[CommunicationResponse] = {

    def dispatchForUpstream(proto: RoutingProtocol, sender: PeerNode): F[CommunicationResponse] =
      proto.message.upstream
        .fold(Log[F].error("Upstream not available").as(notHandled)) { usmsg =>
          usmsg.typeUrl match {
            // TODO interpolate this string to check if class exists
            case "type.googleapis.com/coop.rchain.comm.protocol.rchain.EncryptionHandshake" =>
              handleEncryptionHandshake[F](
                sender,
                EncryptionHandshakeMessage(proto, System.currentTimeMillis))
            // TODO interpolate this string to check if class exists
            case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Frame" =>
              val handled = handleFrame[F](sender, FrameMessage(proto, System.currentTimeMillis))
              errorHandler[F].attempt(handled) >>= {
                case Right(commresponse) => commresponse.pure[F]
                case Left(error) =>
                  Log[F]
                    .error(s"Error occured while handling frame message, error: $error")
                    .as(notHandled)
              }

            case _ =>
              Log[F].error(s"Unexpected message type ${usmsg.typeUrl}") *> notHandled.pure[F]
          }
        }

    msg match {
      case UpstreamMessage(proto, _) =>
        msg.sender.fold(Log[F].error(s"Sender not present, DROPPING msg $msg").as(notHandled)) {
          sender =>
            dispatchForUpstream(proto, sender)
        }
      case upstream @ UpstreamResponse(_, _) =>
        Log[F].debug(s"Out-of-sequence message: $upstream") *> notHandled.pure[F]
      // FIX-ME This MUST be an ERROR, fix it when handlers are rertngin Either[Option]
      case _ => Log[F].warn(s"Unrecognized msg $msg") *> notHandled.pure[F]
    }

  }

  private def getOrError[F[_]: Applicative, A](oa: Option[A], error: CommError)(
      implicit
      err: ApplicativeError_[F, CommError]): F[A] =
    oa.fold[F[A]](err.raiseError[A](error))(_.pure[F])

  def frameMessage[F[_]: Monad: Time: TransportLayer: Encryption](remote: PeerNode,
                                                                  frameable: Nonce => Frameable)(
      implicit keysStore: KeysStore[F],
      err: ApplicativeError_[F, CommError]): F[FrameMessage] =
    frameIt[F](remote, frameable, (local, nonce, f) => frame(local, nonce, f))

  private def frameResponseMessage[F[_]: Monad: Time: TransportLayer: Encryption](
      remote: PeerNode,
      header: Header,
      frameable: Nonce => Frameable)(implicit keysStore: KeysStore[F],
                                     err: ApplicativeError_[F, CommError]): F[FrameMessage] =
    frameIt[F](remote, frameable, (local, nonce, f) => frameResponse(local, header, nonce, f))

  private def frameIt[F[_]: Monad: Time: TransportLayer: Encryption](
      remote: PeerNode,
      frameable: Nonce => Frameable,
      proto: (PeerNode, Nonce, Array[Byte]) => routing.Protocol)(
      implicit keysStore: KeysStore[F],
      err: ApplicativeError_[F, CommError]): F[FrameMessage] =
    for {
      local        <- TransportLayer[F].local
      nonce        <- Encryption[F].generateNonce
      keys         <- Encryption[F].fetchKeys
      remotePubKey <- fetchRemotePublicKey[F](remote)
      framed       = frameable(nonce).toByteArray
      f            <- Encryption[F].encrypt(pub = remotePubKey, sec = keys.priv, nonce, framed)
      ts           <- Time[F].currentMillis
    } yield FrameMessage(proto(local, nonce, f), ts)

  private def fetchRemotePublicKey[F[_]: Monad](remote: PeerNode)(
      implicit keysStore: KeysStore[F],
      err: ApplicativeError_[F, CommError]): F[Key] =
    for {
      maybeKey <- keysStore.get(remote)
      key      <- err.fromEither(maybeKey.toRight(publicKeyNotAvailable(remote)))
    } yield key

  private def errorHandler[F[_]: ErrorHandler]: ErrorHandler[F] = ApplicativeError_[F, CommError]

  private def keysStore[F[_]: KeysStore]: KeysStore[F] = Kvs[F, PeerNode, Array[Byte]]
}
