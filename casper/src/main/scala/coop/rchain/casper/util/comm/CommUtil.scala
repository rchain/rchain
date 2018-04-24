package coop.rchain.casper.util.comm

import cats.Applicative, cats.implicits._

import coop.rchain.casper.protocol.BlockMessage

import coop.rchain.comm.ProtocolMessage
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p.effects.Communication

object CommUtil {
  def sendBlock[F[_]: Applicative: Communication](b: BlockMessage): F[Unit] = {
    val packet = blockMessageToPacket(b)
    val msg    = framePacket(packet)

    val peers = Communication[F].peers
    peers.map(ps => {
      val sends = ps.map(peer => Communication[F].commSend(msg, peer))
      //TODO: Make this log errors in sends
      sends.foldLeft(Applicative[F].pure(Unit))(_ <* _)
    })
  }

  private def blockMessageToPacket(msg: BlockMessage): Packet =
    Packet().withContent(msg.toByteString)

  //TODO: Ask Pawel to provide a method somewhere in Comm that does the framing
  private def framePacket(p: Packet): ProtocolMessage = ???
}
