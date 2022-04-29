package coop.rchain.p2p.effects

import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet

trait PacketHandler[F[_]] {
  def handlePacket(peer: PeerNode, packet: Packet): F[Unit]
}

object PacketHandler {
  def apply[F[_]: PacketHandler]: PacketHandler[F] = implicitly[PacketHandler[F]]
}
