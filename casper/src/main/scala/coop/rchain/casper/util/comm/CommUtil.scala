package coop.rchain.casper.util.comm

import cats.Monad
import cats.implicits._
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter}
import coop.rchain.casper.protocol._
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p.effects._
import coop.rchain.p2p.Network.{frameMessage, ErrorHandler, KeysStore}
import coop.rchain.p2p.NetworkProtocol

import scala.util.Try

object CommUtil {
  def sendBlock[
      F[_]: Monad: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler](
      b: BlockMessage): F[Unit] = {
    val serializedBlock = b.toByteString
    for {
      _     <- Log[F].info(s"CASPER: Beginning send of ${PrettyPrinter.buildString(b)} to peers...")
      peers <- NodeDiscovery[F].peers
      sends <- peers.toList.traverse { peer =>
                frameMessage[F](peer, nonce => NetworkProtocol.framePacket(peer, serializedBlock))
                  .flatMap(msg => TransportLayer[F].commSend(msg, peer).map(_ -> peer))
              }
      _ <- sends.traverse {
            case (Left(err), _) => Log[F].error(s"$err")
            case (Right(_), peer) =>
              Log[F].info(s"CASPER: Sent ${PrettyPrinter.buildString(b)} to $peer")
          }
    } yield ()
  }

  def casperPacketHandler[
      F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler]
    : PartialFunction[Packet, F[String]] =
    Function.unlift(packetToBlockMessage).andThen {
      case b: BlockMessage =>
        for {
          isOldBlock <- MultiParentCasper[F].contains(b)
          logMessage <- if (isOldBlock) {
                         s"CASPER: Received ${PrettyPrinter.buildString(b)} again.".pure[F]
                       } else {
                         handleNewBlock[F](b)
                       }
        } yield logMessage
    }

  private def handleNewBlock[
      F[_]: Monad: MultiParentCasper: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler](
      b: BlockMessage): F[String] =
    for {
      _          <- MultiParentCasper[F].addBlock(b)
      forkchoice <- MultiParentCasper[F].estimator.map(_.head)
      _          <- sendBlock[F](b)
    } yield
      s"CASPER: Received ${PrettyPrinter.buildString(b)}. New fork-choice is ${PrettyPrinter.buildString(forkchoice)}"

  //TODO: Figure out what do with blocks that parse correctly, but are invalid
  private def packetToBlockMessage(msg: Packet): Option[BlockMessage] =
    Try(BlockMessage.parseFrom(msg.content.toByteArray)).toOption
}
