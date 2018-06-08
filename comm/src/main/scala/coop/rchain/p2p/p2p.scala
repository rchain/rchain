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

  type ErrorHandler[F[_]] = ApplicativeError_[F, CommError]

  import CommMessages._

  val defaultTimeout: Duration = Duration(500, MILLISECONDS)

  def findAndConnect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler]
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
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler](
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
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler](
      peer: PeerNode,
      timeout: Duration): F[Unit] = {

    def initProtocolHandshake: F[Unit] =
      for {
        _       <- Log[F].info(s"Initialize protocol handshake to $peer")
        local   <- TransportLayer[F].local
        ph      = ProtocolHandshakeMessage(protocolHandshake(local))
        phsresp <- TransportLayer[F].roundTrip(ph, peer, timeout) >>= errorHandler[F].fromEither
        _       <- Log[F].debug(s"Received protocol handshake response from ${phsresp.sender.get}.")
        _       <- NodeDiscovery[F].addNode(peer)
      } yield ()

    for {
      tss <- Time[F].currentMillis
      _   <- Log[F].debug(s"Connecting to $peer")
      _   <- Metrics[F].incrementCounter("connects")
      _   <- initProtocolHandshake
      tsf <- Time[F].currentMillis
      _   <- Metrics[F].record("connect-time-ms", tsf - tss)
    } yield ()
  }

  def handlePacket[F[_]: Monad: Time: TransportLayer: ErrorHandler: Log: PacketHandler](
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
        local               <- TransportLayer[F].local
        maybeResponsePacket <- PacketHandler[F].handlePacket(remote, p)
        maybeResponsePacketMessage = maybeResponsePacket.map(pr =>
          PacketMessage(ProtocolMessage.upstreamMessage(local, AnyProto.pack(pr))))
      } yield maybeResponsePacketMessage.fold(notHandled)(m => handledWithMessage(m)))
  }

  private def handleProtocolHandshake[
      F[_]: Monad: Time: TransportLayer: NodeDiscovery: Log: ErrorHandler](
      remote: PeerNode,
      maybePh: Option[ProtocolHandshake]): F[CommunicationResponse] =
    for {
      local <- TransportLayer[F].local
      _     <- getOrError[F, ProtocolHandshake](maybePh, parseError("ProtocolHandshake"))
      phr   = ProtocolHandshakeResponseMessage(protocolHandshakeResponse(local))
      _     <- NodeDiscovery[F].addNode(remote)
      _     <- Log[F].info(s"Responded to protocol handshake request from $remote")
    } yield handledWithMessage(phr)

  def dispatch[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: PacketHandler](
      msg: ProtocolMessage): F[CommunicationResponse] = {

    def dispatchForUpstream(proto: RoutingProtocol, sender: PeerNode): F[CommunicationResponse] =
      proto.message.upstream
        .fold(Log[F].error("Upstream not available").as(notHandled)) { usmsg =>
          usmsg.typeUrl match {
            // TODO interpolate this string to check if class exists
            case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Packet" =>
              handlePacket[F](sender, toPacket(proto).toOption)

            case "type.googleapis.com/coop.rchain.comm.protocol.rchain.ProtocolHandshake" =>
              handleProtocolHandshake[F](sender, toProtocolHandshake(proto).toOption)

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
      case _ => Log[F].error(s"Unrecognized msg $msg") *> notHandled.pure[F]
    }
  }

  private def getOrError[F[_]: Applicative: ErrorHandler, A](oa: Option[A],
                                                             error: CommError): F[A] =
    oa.fold[F[A]](errorHandler[F].raiseError[A](error))(_.pure[F])

  private def errorHandler[F[_]: ErrorHandler]: ErrorHandler[F] = ApplicativeError_[F, CommError]
}
