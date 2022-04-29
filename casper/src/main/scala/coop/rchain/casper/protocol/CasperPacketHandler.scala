package coop.rchain.casper.protocol

import cats.FlatMap
import cats.syntax.all._
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.p2p.effects.PacketHandler
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
}
