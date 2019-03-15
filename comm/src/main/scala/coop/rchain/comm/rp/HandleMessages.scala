package coop.rchain.comm.rp

import scala.concurrent.duration._

import cats._
import cats.implicits._
import cats.effect._

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
  private implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.handle")

  def handle[F[_]: Monad: Sync: Log: Time: Metrics: TransportLayer: ErrorHandler: PacketHandler: ConnectionsCell: RPConfAsk](
      protocol: Protocol
  ): F[CommunicationResponse] =
    ProtocolHelper.sender(protocol) match {
      case None =>
        Log[F].error(s"Sender not present, DROPPING $protocol").as(notHandled(senderNotAvailable))
      case Some(sender) => handle_[F](protocol, sender)
    }

  private def handle_[F[_]: Monad: Sync: Log: Time: Metrics: TransportLayer: ErrorHandler: PacketHandler: ConnectionsCell: RPConfAsk](
      proto: Protocol,
      sender: PeerNode
  ): F[CommunicationResponse] =
    proto.message match {
      case Protocol.Message.Heartbeat(heartbeat) => handleHeartbeat[F](sender, heartbeat)
      case Protocol.Message.ProtocolHandshake(protocolhandshake) =>
        handleProtocolHandshake[F](sender, protocolhandshake)
      case Protocol.Message.ProtocolHandshakeResponse(_) =>
        handleProtocolHandshakeResponse[F](sender)
      case Protocol.Message.Disconnect(disconnect) => handleDisconnect[F](sender, disconnect)
      case Protocol.Message.Packet(packet)         => handlePacket[F](sender, packet)
      case msg =>
        Log[F].error(s"Unexpected message type $msg") >> notHandled(unexpectedMessage(msg.toString))
          .pure[F]
    }

  def handleDisconnect[F[_]: Monad: Sync: Metrics: TransportLayer: Log: ConnectionsCell](
      sender: PeerNode,
      disconnect: Disconnect
  ): F[CommunicationResponse] =
    for {
      _ <- Log[F].info(s"Forgetting about ${sender.toAddress}.")
      _ <- TransportLayer[F].disconnect(sender)
      _ <- ConnectionsCell[F].flatModify(_.removeConn[F](sender))
      _ <- Metrics[F].incrementCounter("disconnect")
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

  def handleProtocolHandshakeResponse[F[_]: Monad: Metrics: ConnectionsCell: Log](
      peer: PeerNode
  ): F[CommunicationResponse] =
    for {
      _ <- Log[F].debug(s"Received protocol handshake response from $peer.")
      _ <- ConnectionsCell[F].flatModify(_.addConn[F](peer))
    } yield handledWithoutMessage

  def handleProtocolHandshake[F[_]: Monad: Time: TransportLayer: Log: ErrorHandler: ConnectionsCell: RPConfAsk: Metrics](
      peer: PeerNode,
      protocolHandshake: ProtocolHandshake
  ): F[CommunicationResponse] =
    for {
      local    <- RPConfAsk[F].reader(_.local)
      response = ProtocolHelper.protocolHandshakeResponse(local)
      _        <- TransportLayer[F].send(peer, response) >>= ErrorHandler[F].fromEither
      _        <- Log[F].info(s"Responded to protocol handshake request from $peer")
      _        <- ConnectionsCell[F].flatModify(_.addConn[F](peer))
    } yield handledWithoutMessage

  def handleHeartbeat[F[_]: Monad: Time: TransportLayer: ErrorHandler: RPConfAsk](
      peer: PeerNode,
      heartbeat: Heartbeat
  ): F[CommunicationResponse] =
    RPConfAsk[F].reader(_.local) map (
        local => handledWithMessage(ProtocolHelper.heartbeatResponse(local))
    )

}
