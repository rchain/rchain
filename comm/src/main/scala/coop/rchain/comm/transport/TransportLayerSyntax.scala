package coop.rchain.comm.transport

import cats.Monad
import cats.syntax.all._
import coop.rchain.casper.protocol.ToPacket
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.crypto.Blake2b256Hash

trait TransportLayerSyntax {
  implicit final def commSyntaxTransportLayer[F[_]: Monad: RPConfAsk](
      transport: TransportLayer[F]
  ): TransportLayerOps[F] = new TransportLayerOps[F](transport)
}

final class TransportLayerOps[F[_]: Monad: RPConfAsk](
    // TransportLayer extensions / syntax
    private val transport: TransportLayer[F]
) {
  // Send packet (in one piece)
  def sendToPeer(peer: PeerNode, message: Packet): F[Unit] =
    for {
      conf <- RPConfAsk[F].ask
      msg  = packet(conf.local, conf.networkId, message)
      _    <- transport.send(peer, msg)
    } yield ()

  // Send message (in one piece)
  def sendToPeer[Msg: ToPacket](
      peer: PeerNode,
      message: Msg
  ): F[Unit] = sendToPeer(peer, ToPacket(message))

  // Send packet in chunks (stream)
  def streamToPeer(peer: PeerNode, packet: Packet): F[Unit] =
    for {
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, packet)
      _     <- transport.stream(peer, msg)
    } yield ()

  // Send message in chunks (stream)
  def streamToPeer[Msg: ToPacket](
      peer: PeerNode,
      message: Msg
  ): F[Unit] = streamToPeer(peer, ToPacket(message))

  def sendToBootstrap[Msg: ToPacket](message: Msg): F[Unit] =
    for {
      maybeBootstrap <- RPConfAsk[F].reader(_.bootstrap)
      bootstrap      = maybeBootstrap.get
      _              <- sendToPeer(bootstrap, message)
    } yield ()
}
