package coop.rchain.casper.util.comm

import cats._
import cats.effect.Concurrent
import cats.syntax.all._

import coop.rchain.casper.engine._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine.Running.RequestedBlocks
import coop.rchain.casper.protocol._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.p2p.effects._
import coop.rchain.shared.Log

object CasperPacketHandler {

  def apply[F[_]: FlatMap: EngineCell: Log]: PacketHandler[F] =
    (peer: PeerNode, packet: Packet) =>
      toCasperMessageProto(packet).toEither
        .flatMap(proto => CasperMessage.from(proto))
        .fold(
          err => Log[F].warn(s"Could not extract casper message from packet sent by $peer: $err"),
          message => EngineCell[F].read >>= (_.handle(peer, message))
        )

  def fairDispatcher[F[_]: Concurrent: EngineCell: Running.RequestedBlocks: Log](
      maxPeerQueueSize: Int,
      giveUpAfterSkipped: Int,
      dropPeerAfterRetries: Int
  ): F[PacketHandler[F]] = {
    import FairRoundRobinDispatcher._
    implicit val showPeerNode: Show[PeerNode] = _.toString
    implicit val showCasperMessage: Show[CasperMessage] = {
      case BlockHashMessage(h) => s"[${PrettyPrinter.buildString(h)}]"
      case m: CasperMessage    => s"[Unexpected ${m.getClass.getSimpleName}!!!]"
    }

    def checkMessage(message: CasperMessage): F[Dispatch] =
      message match {
        case msg: BlockHashMessage =>
          for {
            engine    <- EngineCell[F].read
            contains  <- engine.withCasper(_.contains(msg.blockHash), false.pure[F])
            requested <- RequestedBlocks.contains(msg.blockHash)
          } yield if (contains || requested) Dispatch.drop else Dispatch.handle

        case _ => Dispatch.pass.pure[F]
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
