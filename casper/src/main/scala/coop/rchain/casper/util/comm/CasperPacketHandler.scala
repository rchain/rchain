package coop.rchain.casper.util.comm

import coop.rchain.casper._
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.protocol.toCasperMessage
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.p2p.effects._
import coop.rchain.shared.Log
import cats._, cats.data._, cats.implicits._

object CasperPacketHandler {
  def apply[F[_]: FlatMap: EngineCell: Log]: PacketHandler[F] =
    new PacketHandler[F] {
      def handlePacket(peer: PeerNode, packet: Packet): F[Unit] =
        toCasperMessage(packet).fold(
          Log[F].warn(s"Could not extract casper message from packet sent by $peer")
        ) {
          case message => EngineCell[F].read >>= (_.handle(peer, message))
        }
    }
}
