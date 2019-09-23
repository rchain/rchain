package coop.rchain.casper.util.comm

import coop.rchain.casper._
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.p2p.effects._
import coop.rchain.shared.Log
import cats._, cats.data._, cats.implicits._

object CasperPacketHandler {
  def apply[F[_]: FlatMap: EngineCell: Log]: PacketHandler[F] =
    new PacketHandler[F] {
      def handlePacket(peer: PeerNode, packet: Packet): F[Unit] =
        toCasperMessage(toCasperMessageProto(packet), peer: PeerNode)
          .fold(err => Log[F].warn(err), {
            case message => EngineCell[F].read >>= (_.handle(peer, message))
          })
    }

  def toCasperMessage(
      maybeCMP: Option[CasperMessageProto],
      peer: PeerNode
  ): Either[String, CasperMessage] = maybeCMP match {
    case None      => Left(s"Could not extract casper message from packet sent by $peer")
    case Some(cmp) => CasperMessage.from(cmp)
  }
}
