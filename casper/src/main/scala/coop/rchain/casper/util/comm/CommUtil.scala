package coop.rchain.casper.util.comm

import cats.Monad, cats.implicits._

import coop.rchain.casper.protocol.BlockMessage

import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.ProtocolMessage
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p.effects.{Communication, Log}

object CommUtil {
  def sendBlock[F[_]: Monad: Communication: Log](b: BlockMessage): F[Unit] = {
    val packet = blockMessageToPacket(b)
    val msg    = framePacket(packet)

    for {
      peers <- Communication[F].peers
      sends <- peers.toList.traverse(peer => Communication[F].commSend(msg, peer))
      _ <- sends.traverse {
            case Left(err) => Log[F].error(s"$err")
            case Right(_)  => Monad[F].pure(())
          }
    } yield ()
  }

  private def blockMessageToPacket(msg: BlockMessage): Packet =
    Packet().withContent(msg.toByteString)

  //TODO: Ask Pawel to provide a method somewhere in Comm that does the framing
  private def framePacket(p: Packet): ProtocolMessage = ???
}
