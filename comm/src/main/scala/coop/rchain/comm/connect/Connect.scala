package coop.rchain.comm.connect

import coop.rchain.p2p.effects._

import coop.rchain.comm.discovery._
import scala.concurrent.duration._
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.routing, routing.Header
import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing.{Protocol => RoutingProtocol}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.metrics.Metrics

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.comm.transport._, CommunicationResponse._, CommMessages._
import coop.rchain.shared._
import coop.rchain.comm.CommError.ErrorHandler

object Connect {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def findAndConnect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler](
      defaultTimeout: FiniteDuration): Int => F[Int] =
    (lastCount: Int) =>
      for {
        _         <- IOUtil.sleep[F](5000L)
        peers     <- NodeDiscovery[F].findMorePeers(10).map(_.toList)
        responses <- peers.traverse(connect[F](_, defaultTimeout).attempt)
        _ <- peers.zip(responses).traverse {
              case (peer, Left(error)) =>
                Log[F].error(s"Failed to connect to $peer. Reason: $error")
              case (_, Right(_)) => ().pure[F]
            }
        thisCount <- NodeDiscovery[F].peers.map(_.size)
        _         <- (thisCount != lastCount).fold(Log[F].info(s"Peers: $thisCount."), ().pure[F])
      } yield thisCount

  def connectToBootstrap[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler](
      bootstrapAddrStr: String,
      maxNumOfAttempts: Int = 5,
      defaultTimeout: FiniteDuration): F[Unit] = {

    def connectAttempt(attempt: Int, timeout: FiniteDuration, bootstrapAddr: PeerNode): F[Unit] =
      if (attempt > maxNumOfAttempts) for {
        _ <- Log[F].error("Failed to connect to bootstrap node, exiting...")
        _ <- errorHandler[F].raiseError[Unit](couldNotConnectToBootstrap)
      } yield ()
      else
        for {
          res <- connect[F](bootstrapAddr, timeout).attempt
          _ <- res match {
                case Left(error) =>
                  val msg =
                    s"Failed to connect to bootstrap (attempt $attempt / $maxNumOfAttempts). Reason: $error"
                  Log[F].warn(msg) *> connectAttempt(attempt + 1,
                                                     timeout + defaultTimeout,
                                                     bootstrapAddr)
                case Right(_) => ().pure[F]
              }
        } yield ()

    for {
      bootstrapAddr <- errorHandler[F].fromEither(PeerNode.parse(bootstrapAddrStr))
      _             <- Log[F].info(s"Bootstrapping from $bootstrapAddr.")
      _             <- connectAttempt(attempt = 1, defaultTimeout, bootstrapAddr)
      _             <- Log[F].info(s"Connected $bootstrapAddr.")
    } yield ()
  }

  def connect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler](
      peer: PeerNode,
      timeout: FiniteDuration): F[Unit] =
    for {
      tss     <- Time[F].currentMillis
      _       <- Log[F].debug(s"Connecting to $peer")
      _       <- Metrics[F].incrementCounter("connects")
      _       <- Log[F].info(s"Initialize protocol handshake to $peer")
      local   <- TransportLayer[F].local
      ph      = protocolHandshake(local)
      phsresp <- TransportLayer[F].roundTrip(peer, ph, timeout) >>= errorHandler[F].fromEither
      _ <- Log[F].debug(
            s"Received protocol handshake response from ${ProtocolHelper.sender(phsresp)}.")
      addedErr <- NodeDiscovery[F].addNode(peer)
      _ <- addedErr.fold(
            error => Log[F].error(s"Could not add $peer, reason: $error"),
            _ => ().pure[F]
          )
      tsf <- Time[F].currentMillis
      _   <- Metrics[F].record("connect-time-ms", tsf - tss)
    } yield ()

  def handlePacket[F[_]: Monad: Time: TransportLayer: ErrorHandler: Log: PacketHandler](
      remote: PeerNode,
      maybePacket: Option[Packet]): F[CommunicationResponse] = {
    val errorMsg = s"Expecting Packet from frame, got something else. Stopping the node."
    def handleNone: F[CommunicationResponse] =
      for {
        _     <- Log[F].error(errorMsg)
        error = unknownCommError(errorMsg)
        _     <- errorHandler[F].raiseError[Unit](error)
      } yield notHandled(error)

    maybePacket.fold(handleNone)(
      p =>
        for {
          local               <- TransportLayer[F].local
          maybeResponsePacket <- PacketHandler[F].handlePacket(remote, p)
          maybeResponsePacketMessage = maybeResponsePacket.map(pr =>
            ProtocolHelper.upstreamMessage(local, AnyProto.pack(pr)))
        } yield
          maybeResponsePacketMessage.fold(notHandled(noResponseForRequest))(m =>
            handledWithMessage(m)))
  }

  private def handleProtocolHandshake[
      F[_]: Monad: Time: TransportLayer: NodeDiscovery: Log: ErrorHandler](
      peer: PeerNode,
      maybePh: Option[ProtocolHandshake]): F[CommunicationResponse] =
    for {
      local    <- TransportLayer[F].local
      _        <- getOrError[F, ProtocolHandshake](maybePh, parseError("ProtocolHandshake"))
      phr      = protocolHandshakeResponse(local)
      addedErr <- NodeDiscovery[F].addNode(peer)
      commResponse <- addedErr.fold(
                       error =>
                         Log[F].error(s"Could not add $peer, reason: $error").as(notHandled(error)),
                       _ =>
                         Log[F]
                           .info(s"Responded to protocol handshake request from $peer")
                           .as(handledWithMessage(phr))
                     )
    } yield commResponse

  def dispatch[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: PacketHandler](
      protocol: RoutingProtocol): F[CommunicationResponse] = {

    def dispatchForUpstream(proto: RoutingProtocol, sender: PeerNode): F[CommunicationResponse] =
      proto.message.upstream
        .fold(Log[F].error("Upstream not available").as(notHandled(upstreamNotAvailable))) {
          usmsg =>
            usmsg.typeUrl match {
              // TODO interpolate this string to check if class exists

              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Packet" =>
                handlePacket[F](sender, toPacket(proto).toOption)

              case "type.googleapis.com/coop.rchain.comm.protocol.rchain.ProtocolHandshake" =>
                handleProtocolHandshake[F](sender, toProtocolHandshake(proto).toOption)

              case _ =>
                Log[F].error(s"Unexpected message type ${usmsg.typeUrl}") *> notHandled(
                  unexpectedMessage(usmsg.typeUrl)).pure[F]
            }
        }

    ProtocolHelper
      .sender(protocol)
      .fold(
        Log[F]
          .error(s"Sender not present, DROPPING $protocol")
          .as(notHandled(senderNotAvailable))) { sender =>
        dispatchForUpstream(protocol, sender)
      }
  }

  private def getOrError[F[_]: Applicative: ErrorHandler, A](oa: Option[A],
                                                             error: CommError): F[A] =
    oa.fold[F[A]](errorHandler[F].raiseError[A](error))(_.pure[F])

  private def errorHandler[F[_]: ErrorHandler]: ErrorHandler[F] = ApplicativeError_[F, CommError]
}
