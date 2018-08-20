package coop.rchain.comm.rp

import cats._
import cats.effect.Timer
import cats.implicits._
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.rchain._
import coop.rchain.comm.protocol.routing.{Protocol => RoutingProtocol}
import coop.rchain.comm.rp.Connect.Connections._
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.CommMessages._
import coop.rchain.comm.transport.CommunicationResponse._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects._
import coop.rchain.shared._

import scala.concurrent.duration._

object HandleMessages {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def handle[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: ErrorHandler: PacketHandler: ConnectionsCell: RPConfAsk](
      protocol: RoutingProtocol,
      defaultTimeout: FiniteDuration): F[CommunicationResponse] =
    ProtocolHelper.sender(protocol) match {
      case None =>
        Log[F].error(s"Sender not present, DROPPING $protocol").as(notHandled(senderNotAvailable))
      case Some(sender) => handle_[F](protocol, sender, defaultTimeout)
    }

  private def handle_[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: ErrorHandler: PacketHandler: ConnectionsCell: RPConfAsk](
      proto: RoutingProtocol,
      sender: PeerNode,
      defaultTimeout: FiniteDuration): F[CommunicationResponse] =
    proto.message.upstream
      .fold(Log[F].error("Upstream not available").as(notHandled(upstreamNotAvailable))) { usmsg =>
        usmsg.typeUrl match {
          // TODO interpolate this string to check if class exists
          case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Heartbeat" =>
            handleHeartbeat[F](sender, toHeartbeat(proto).toOption)
          case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Packet" =>
            handlePacket[F](sender, toPacket(proto).toOption)
          case "type.googleapis.com/coop.rchain.comm.protocol.rchain.ProtocolHandshake" =>
            handleProtocolHandshake[F](sender, toProtocolHandshake(proto).toOption, defaultTimeout)
          case "type.googleapis.com/coop.rchain.comm.protocol.rchain.Disconnect" =>
            handleDisconnect[F](sender, toDisconnect(proto).toOption)
          case _ =>
            Log[F].error(s"Unexpected message type ${usmsg.typeUrl}") *> notHandled(
              unexpectedMessage(usmsg.typeUrl)).pure[F]
        }
      }

  def handleDisconnect[F[_]: Monad: Capture: Metrics: TransportLayer: Log: ConnectionsCell](
      sender: PeerNode,
      maybeDisconnect: Option[Disconnect]): F[CommunicationResponse] = {

    val errorMsg = s"Expecting Disconnect, got something else."
    def handleNone: F[CommunicationResponse] =
      Log[F].error(errorMsg).as(notHandled(unknownCommError(errorMsg)))

    maybeDisconnect.fold(handleNone)(disconnect =>
      for {
        _ <- Log[F].info(s"Forgetting about ${sender.toAddress}.")
        _ <- TransportLayer[F].disconnect(sender)
        _ <- ConnectionsCell[F].modify(_.removeConn[F](sender))
        _ <- Metrics[F].incrementCounter("disconnect-recv-count")
      } yield handledWithoutMessage)

  }

  def handlePacket[F[_]: Monad: Time: TransportLayer: ErrorHandler: Log: PacketHandler: RPConfAsk](
      remote: PeerNode,
      maybePacket: Option[Packet]): F[CommunicationResponse] = {
    val errorMsg = s"Expecting Packet, got something else. Stopping the node."
    def handleNone: F[CommunicationResponse] =
      for {
        _     <- Log[F].error(errorMsg)
        error = unknownCommError(errorMsg)
        _     <- ErrorHandler[F].raiseError[Unit](error)
      } yield notHandled(error)

    maybePacket.fold(handleNone)(
      p =>
        for {
          local               <- RPConfAsk[F].reader(_.local)
          maybeResponsePacket <- PacketHandler[F].handlePacket(remote, p)
          maybeResponsePacketMessage = maybeResponsePacket.map(pr =>
            ProtocolHelper.upstreamMessage(local, AnyProto.pack(pr)))
        } yield
          maybeResponsePacketMessage.fold(notHandled(noResponseForRequest))(m =>
            handledWithMessage(m)))
  }

  def handleProtocolHandshake[
      F[_]: Monad: Time: TransportLayer: Log: ErrorHandler: ConnectionsCell: RPConfAsk: Metrics](
      peer: PeerNode,
      maybePh: Option[ProtocolHandshake],
      defaultTimeout: FiniteDuration
  ): F[CommunicationResponse] = {

    def notHandledHandshake(error: CommError): F[CommunicationResponse] =
      Log[F]
        .warn(s"Not adding. Could receive Pong message back from $peer, reason: $error")
        .as(notHandled(error))

    def handledHandshake(local: PeerNode): F[CommunicationResponse] =
      for {
        _ <- ConnectionsCell[F].modify(_.addConn[F](peer))
        _ <- Log[F].info(s"Responded to protocol handshake request from $peer")
      } yield handledWithMessage(protocolHandshakeResponse(local))

    for {
      local        <- RPConfAsk[F].reader(_.local)
      _            <- getOrError[F, ProtocolHandshake](maybePh, parseError("ProtocolHandshake"))
      hbrErr       <- TransportLayer[F].roundTrip(peer, heartbeat(local), defaultTimeout)
      commResponse <- hbrErr.fold(error => notHandledHandshake(error), _ => handledHandshake(local))
    } yield commResponse
  }

  def handleHeartbeat[F[_]: Monad: Time: TransportLayer: ErrorHandler: RPConfAsk](
      peer: PeerNode,
      maybeHeartbeat: Option[Heartbeat]): F[CommunicationResponse] =
    for {
      local <- RPConfAsk[F].reader(_.local)
      _     <- getOrError[F, Heartbeat](maybeHeartbeat, parseError("Heartbeat"))
    } yield handledWithMessage(heartbeatResponse(local))

  private def getOrError[F[_]: Applicative: ErrorHandler, A](oa: Option[A],
                                                             error: CommError): F[A] =
    oa.fold[F[A]](ErrorHandler[F].raiseError[A](error))(_.pure[F])

}
