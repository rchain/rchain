package coop.rchain.casper.util.comm

import cats._
import cats.effect.Concurrent
import cats.syntax.all._

import coop.rchain.casper.engine._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.p2p.effects._
import coop.rchain.shared.Log

object CasperPacketHandler {

  def apply[F[_]: FlatMap: EngineCell: Log]: PacketHandler[F] =
    (peer: PeerNode, packet: Packet) =>
      (toCasperMessageProto(packet).toEither
        .flatMap(proto => CasperMessage.from(proto)))
        .fold(
          err => Log[F].warn(s"Could not extract casper message from packet sent by $peer: $err"),
          message => EngineCell[F].read >>= (_.handle(peer, message))
        )

  def fairDispatcher[F[_]: Concurrent: EngineCell: Log](
      maxPeerQueueSize: Int,
      giveUpAfterSkipped: Int,
      dropPeerAfterRetries: Int
  ): F[PacketHandler[F]] = {
    import FairRoundRobinDispatcher._
    implicit val showPeerNode: Show[PeerNode] = _.toString

    def checkMessage(message: CasperMessage): F[Dispatch] =
      message match {
        case _: BlockHashMessage => Dispatch.handle.pure[F]
        case _                   => Dispatch.pass.pure[F]
      }

    def handle(peer: PeerNode, message: CasperMessage): F[Unit] =
      EngineCell[F].read >>= (_.handle(peer, message))

    FairRoundRobinDispatcher[F, PeerNode, CasperMessage](
      checkMessage,
      handle,
      maxPeerQueueSize,
      giveUpAfterSkipped,
      dropPeerAfterRetries
    ).map { dispatcher => (peer, packet) =>
      toCasperMessageProto(packet).toEither
        .flatMap(proto => CasperMessage.from(proto))
        .fold(
          err => Log[F].warn(s"Could not extract casper message from packet sent by $peer: $err"),
          dispatcher.dispatch(peer, _)
        )
    }
  }
}
