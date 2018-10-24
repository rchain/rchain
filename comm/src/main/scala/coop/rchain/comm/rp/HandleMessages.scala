package coop.rchain.comm.rp

import scala.concurrent.duration._

import cats._
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.comm._
import coop.rchain.comm.CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.Connect.Connections._
import coop.rchain.comm.transport._
import coop.rchain.comm.transport.CommunicationResponse._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects._
import coop.rchain.shared._

object HandleMessages {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def handle[F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: ErrorHandler: PacketHandler: ConnectionsCell: RPConfAsk](
      protocol: Protocol,
      defaultTimeout: FiniteDuration
  ): F[CommunicationResponse] =
    ProtocolHelper.sender(protocol) match {
      case None =>
        Log[F].error(s"Sender not present, DROPPING $protocol").as(notHandled(senderNotAvailable))
      case Some(sender) => handle_[F](protocol, sender, defaultTimeout)
    }

  private def handle_[F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: ErrorHandler: PacketHandler: ConnectionsCell: RPConfAsk](
      proto: Protocol,
      sender: PeerNode,
      defaultTimeout: FiniteDuration
  ): F[CommunicationResponse] =
    proto.message match {
      case Protocol.Message.Heartbeat(heartbeat) => handleHeartbeat[F](sender, heartbeat)
      case Protocol.Message.ProtocolHandshake(protocolhandshake) =>
        handleProtocolHandshake[F](sender, protocolhandshake, defaultTimeout)
      case Protocol.Message.Disconnect(disconnect) => handleDisconnect[F](sender, disconnect)
      case Protocol.Message.Packet(packet)         => handlePacket[F](sender, packet)
      case msg =>
        Log[F].error(s"Unexpected message type $msg") *> notHandled(unexpectedMessage(msg.toString))
          .pure[F]
    }

  def handleDisconnect[F[_]: Monad: Capture: Metrics: TransportLayer: Log: ConnectionsCell](
      sender: PeerNode,
      disconnect: Disconnect
  ): F[CommunicationResponse] =
    for {
      _ <- Log[F].info(s"Forgetting about ${sender.toAddress}.")
      _ <- TransportLayer[F].disconnect(sender)
      _ <- ConnectionsCell[F].modify(_.removeConn[F](sender))
      _ <- Metrics[F].incrementCounter("disconnect-recv-count")
    } yield handledWithoutMessage

  def handlePacket[F[_]: Monad: Time: TransportLayer: ErrorHandler: Log: PacketHandler: RPConfAsk](
      remote: PeerNode,
      packet: Packet
  ): F[CommunicationResponse] =
    for {
      local               <- RPConfAsk[F].reader(_.local)
      maybeResponsePacket <- PacketHandler[F].handlePacket(remote, packet)
    } yield
      maybeResponsePacket
        .fold(notHandled(noResponseForRequest))(
          m => handledWithMessage(ProtocolHelper.protocol(local).withPacket(m))
        )

  def handleProtocolHandshake[F[_]: Monad: Time: TransportLayer: Log: ErrorHandler: ConnectionsCell: RPConfAsk: Metrics](
      peer: PeerNode,
      protocolHandshake: ProtocolHandshake,
      defaultTimeout: FiniteDuration
  ): F[CommunicationResponse] = {

    def notHandledHandshake(error: CommError): F[CommunicationResponse] =
      Log[F]
        .warn(s"Not adding. Could not receive response to heartbeat from $peer, reason: $error")
        .as(notHandled(error))

    def handledHandshake(local: PeerNode): F[CommunicationResponse] =
      for {
        _ <- ConnectionsCell[F].modify(_.addConn[F](peer))
        _ <- Log[F].info(s"Responded to protocol handshake request from $peer")
      } yield handledWithMessage(ProtocolHelper.protocolHandshakeResponse(local))

    for {
      local        <- RPConfAsk[F].reader(_.local)
      hbrErr       <- TransportLayer[F].roundTrip(peer, ProtocolHelper.heartbeat(local), defaultTimeout)
      commResponse <- hbrErr.fold(error => notHandledHandshake(error), _ => handledHandshake(local))
    } yield commResponse
  }

  def handleHeartbeat[F[_]: Monad: Time: TransportLayer: ErrorHandler: RPConfAsk](
      peer: PeerNode,
      heartbeat: Heartbeat
  ): F[CommunicationResponse] =
    RPConfAsk[F].reader(_.local) map (
        local => handledWithMessage(ProtocolHelper.heartbeatResponse(local))
    )

}
