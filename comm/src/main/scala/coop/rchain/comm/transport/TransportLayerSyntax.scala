package coop.rchain.comm.transport

import cats.Monad
import cats.syntax.all._
import coop.rchain.casper.protocol.ToPacket
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper.packet

trait TransportLayerSyntax {
  implicit final def commSyntaxTransportLayer[F[_]](
      transport: TransportLayer[F]
  ): TransportLayerOps[F] = new TransportLayerOps[F](transport)
}

final class TransportLayerOps[F[_]](
    // TransportLayer extensions / syntax
    private val transport: TransportLayer[F]
) extends AnyVal {
  def stream1(peer: PeerNode, blob: Blob): F[Unit] =
    transport.stream(Seq(peer), blob)

  // Send packet (in one piece)
  def sendToPeer(peer: PeerNode, message: Packet)(implicit m: Monad[F], c: RPConfAsk[F]): F[Unit] =
    for {
      conf <- RPConfAsk[F].ask
      msg  = packet(conf.local, conf.networkId, message)
      _    <- transport.send(peer, msg)
    } yield ()

  // Send message (in one piece)
  def sendToPeer[Msg: ToPacket](
      peer: PeerNode,
      message: Msg
  )(implicit m: Monad[F], c: RPConfAsk[F]): F[Unit] = sendToPeer(peer, ToPacket(message))

  // Send packet in chunks (stream)
  def streamToPeer(
      peer: PeerNode,
      packet: Packet
  )(implicit m: Monad[F], c: RPConfAsk[F]): F[Unit] =
    for {
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, packet)
      _     <- stream1(peer, msg)
    } yield ()

  // Send message in chunks (stream)
  def streamToPeer[Msg: ToPacket](
      peer: PeerNode,
      message: Msg
  )(implicit m: Monad[F], c: RPConfAsk[F]): F[Unit] = streamToPeer(peer, ToPacket(message))

  def sendToBootstrap[Msg: ToPacket](message: Msg)(implicit m: Monad[F], c: RPConfAsk[F]): F[Unit] =
    for {
      maybeBootstrap <- RPConfAsk[F].reader(_.bootstrap)
      bootstrap      = maybeBootstrap.get
      _              <- sendToPeer(bootstrap, message)
    } yield ()
}
