package coop.rchain.comm.rp

import cats._
import cats.effect._
import cats.syntax.all._

import coop.rchain.catscontrib.ski._
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

  implicit private val logSource: LogSource = LogSource(this.getClass)
  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.handle")

  def handle[F[_]: Monad: Sync: Log: Time: Metrics: TransportLayer: PacketHandler: ConnectionsCell: RPConfAsk](
      protocol: Protocol
  ): F[CommunicationResponse] = handle_[F](protocol, ProtocolHelper.sender(protocol))

  private def handle_[F[_]: Monad: Sync: Log: Time: Metrics: TransportLayer: PacketHandler: ConnectionsCell: RPConfAsk](
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
      _ <- ConnectionsCell[F].flatModify(_.removeConnAndReport[F](sender))
      _ <- Metrics[F].incrementCounter("disconnect")
    } yield handledWithoutMessage

  def handlePacket[F[_]: Monad: Time: TransportLayer: Log: PacketHandler: RPConfAsk](
      remote: PeerNode,
      packet: Packet
  ): F[CommunicationResponse] =
    for {
      conf <- RPConfAsk[F].ask
      _    <- PacketHandler[F].handlePacket(remote, packet)
    } yield handledWithoutMessage

  def handleProtocolHandshakeResponse[F[_]: Monad: TransportLayer: Metrics: ConnectionsCell: Log: RPConfAsk](
      peer: PeerNode
  ): F[CommunicationResponse] =
    for {
      _ <- Log[F].debug(s"Received protocol handshake response from $peer.")
      _ <- ConnectionsCell[F].flatModify(_.addConnAndReport[F](peer))
    } yield handledWithoutMessage

  def handleProtocolHandshake[F[_]: Monad: TransportLayer: Log: ConnectionsCell: RPConfAsk: Metrics](
      peer: PeerNode,
      protocolHandshake: ProtocolHandshake
  ): F[CommunicationResponse] =
    for {
      conf     <- RPConfAsk[F].ask
      response = ProtocolHelper.protocolHandshakeResponse(conf.local, conf.networkId)
      resErr   <- TransportLayer[F].send(peer, response)
      _ <- resErr.fold(
            kp(().pure[F]),
            kp(
              Log[F].info(s"Responded to protocol handshake request from $peer") >>
                ConnectionsCell[F].flatModify(_.addConnAndReport[F](peer))
            )
          )
    } yield handledWithoutMessage

  def handleHeartbeat[F[_]: Monad: ConnectionsCell](
      peer: PeerNode,
      heartbeat: Heartbeat
  ): F[CommunicationResponse] =
    ConnectionsCell[F]
      .flatModify(_.refreshConn[F](peer))
      .map(kp(handledWithoutMessage))
}
