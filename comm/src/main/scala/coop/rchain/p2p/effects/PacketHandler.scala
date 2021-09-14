package coop.rchain.p2p.effects

import cats.syntax.all._
import cats.{Applicative, ApplicativeError, FlatMap}
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.shared._

trait PacketHandler[F[_]] {
  def handlePacket(peer: PeerNode, packet: Packet): F[Unit]
}

object PacketHandler {
  implicit private val logSource: LogSource = LogSource(this.getClass)

  def apply[F[_]: PacketHandler]: PacketHandler[F] = implicitly[PacketHandler[F]]

  def pf[F[_]](pfForPeer: (PeerNode) => PartialFunction[Packet, F[Unit]])(
      implicit ev1: FlatMap[F],
      ev2: Log[F],
      errorHandler: ApplicativeError[F, CommError]
  ): PacketHandler[F] =
    new PacketHandler[F] {
      def handlePacket(peer: PeerNode, packet: Packet): F[Unit] = {
        val errorMsg = s"Unable to handle packet $packet"
        val pf       = pfForPeer(peer)
        if (pf.isDefinedAt(packet)) pf(packet)
        else
          Log[F].error(errorMsg) >> errorHandler
            .raiseError(CommError.unknownProtocol(errorMsg))
      }
    }

  class NOPPacketHandler[F[_]: Applicative] extends PacketHandler[F] {
    def handlePacket(peer: PeerNode, packet: Packet): F[Unit] = ().pure[F]
  }

}
