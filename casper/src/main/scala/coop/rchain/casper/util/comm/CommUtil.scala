package coop.rchain.casper.util.comm

import cats.{Applicative, Monad}, cats.implicits._

import coop.rchain.casper.protocol.BlockMessage

import coop.rchain.comm.ProtocolMessage
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p.effects.{Communication, Log}

import coop.rchain.crypto.codec.Base16

import scala.util.Try

object CommUtil {
  def sendBlock[F[_]: Monad: Communication: Log](b: BlockMessage): F[Unit] = {
    val packet = blockMessageToPacket(b)
    val msg    = framePacket(packet)

    for {
      peers <- Communication[F].peers
      sends <- peers.toList.traverse(peer => Communication[F].commSend(msg, peer).map(_ -> peer))
      _ <- sends.traverse {
            case (Left(err), _)   => Log[F].error(s"$err")
            case (Right(_), peer) => Log[F].info(s"Sent block ${hashString(b)} to $peer")
          }
    } yield ()
  }

  def casperPacketHandler[F[_]: Applicative]: PartialFunction[Packet, F[String]] =
    Function.unlift(packetToBlockMessage).andThen {
      case b: BlockMessage =>
        /*
         * TODO:
         *  -add new block to internal Casper protocol state
         *  -run fork-choice
         *  -if this is a block that has not been recieved before then send to known peers
         */
        "I got a block :)".pure[F]
    }

  private def hashString(b: BlockMessage): String =
    Base16.encode(b.blockHash.toByteArray)

  private def blockMessageToPacket(msg: BlockMessage): Packet =
    Packet().withContent(msg.toByteString)

  private def packetToBlockMessage(msg: Packet): Option[BlockMessage] =
    Try(BlockMessage.parseFrom(msg.content.toByteArray)).toOption

  //TODO: Ask Pawel to provide a method somewhere in Comm that does the framing
  private def framePacket(p: Packet): ProtocolMessage = ???
}
