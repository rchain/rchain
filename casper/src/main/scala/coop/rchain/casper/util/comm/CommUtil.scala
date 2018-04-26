package coop.rchain.casper.util.comm

import cats.{Applicative, Monad}
import cats.implicits._

import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.comm.ProtocolMessage
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p.effects.{Log, NodeDiscovery, TransportLayer}
import coop.rchain.crypto.codec.Base16
import scala.util.Try

object CommUtil {
  def sendBlock[F[_]: Monad: NodeDiscovery: TransportLayer: Log](b: BlockMessage): F[Unit] = {
    val packet = blockMessageToPacket(b)
    val msg    = framePacket(packet)

    for {
      peers <- NodeDiscovery[F].peers
      sends <- peers.toList.traverse(peer => TransportLayer[F].commSend(msg, peer).map(_ -> peer))
      _ <- sends.traverse {
            case (Left(err), _)   => Log[F].error(s"$err")
            case (Right(_), peer) => Log[F].info(s"Sent block ${hashString(b)} to $peer")
          }
    } yield ()
  }

  def casperPacketHandler[F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log]
    : PartialFunction[Packet, F[String]] =
    Function.unlift(packetToBlockMessage).andThen {
      case b: BlockMessage =>
        for {
          isNewBlock <- MultiParentCasper[F].contains(b)
          logMessage <- if (isNewBlock) {
                         handleNewBlock[F](b)
                       } else {
                         s"Received block ${hashString(b)} again.".pure[F]
                       }
        } yield logMessage
    }

  private def handleNewBlock[F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log](
      b: BlockMessage): F[String] =
    for {
      _          <- MultiParentCasper[F].addBlock(b)
      forkchoice <- MultiParentCasper[F].estimator.map(_.head)
      _          <- sendBlock[F](b)
    } yield s"Received block ${hashString(b)}; new fork-choice is ${hashString(forkchoice)}."

  private def hashString(b: BlockMessage): String =
    Base16.encode(b.blockHash.toByteArray)

  //TODO: Figure out what do with blocks that parse correctly, but are invalid
  private def blockMessageToPacket(msg: BlockMessage): Packet =
    Packet().withContent(msg.toByteString)

  private def packetToBlockMessage(msg: Packet): Option[BlockMessage] =
    Try(BlockMessage.parseFrom(msg.content.toByteArray)).toOption

  //TODO: Ask Pawel to provide a method somewhere in Comm that does the framing
  private def framePacket(p: Packet): ProtocolMessage = ???
}
