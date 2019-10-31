package coop.rchain.casper.util.comm

import cats._
import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
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

import com.google.protobuf.ByteString

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
    implicit def showSourceHolder: Show[BlockCreator] = sh => s"[${sh.toString}]"
    implicit val showCasperMessage: Show[(PeerNode, CasperMessage)] = {
      case (_, BlockHashMessage(h, v)) =>
        s"[${PrettyPrinter.buildString(h)}]@[${PrettyPrinter.buildString(v)}]"
      case (peer, m: CasperMessage) =>
        s"[Unexpected message ${m.getClass.getSimpleName} from $peer!!!]"
    }

    def checkMessage(message: (PeerNode, CasperMessage)): F[Dispatch] =
      message match {
        case (_, msg: BlockHashMessage) =>
          for {
            engine    <- EngineCell[F].read
            contains  <- engine.withCasper(_.contains(msg.blockHash), false.pure[F])
            requested <- RequestedBlocks.contains(msg.blockHash)
          } yield if (contains || requested) Dispatch.drop else Dispatch.handle

        case _ => Dispatch.pass.pure[F]
      }

    def handle(holder: BlockCreator, message: (PeerNode, CasperMessage)): F[Unit] =
      EngineCell[F].read >>= (_.handle(message._1, message._2))

    Semaphore[F](1) >>= { lock =>
      FairRoundRobinDispatcher[F, BlockCreator, (PeerNode, CasperMessage)](
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
            msg => lock.withPermit(dispatcher.dispatch(BlockCreator(msg), (peer, msg)))
          )
      }
    }
  }

  private class BlockCreator(val value: ByteString) {
    override lazy val hashCode: Int = value.hashCode()
    override def equals(obj: Any): Boolean =
      obj match {
        case other: BlockCreator => other.hashCode == hashCode && other.value.equals(value)
        case _                   => false
      }
    override lazy val toString: String = PrettyPrinter.buildString(value)
  }

  private object BlockCreator {
    def apply(message: CasperMessage): BlockCreator = {
      val value =
        message match {
          case BlockHashMessage(_, bc) => bc
          case _                       => ByteString.EMPTY
        }
      new BlockCreator(value)
    }
  }
}
