package coop.rchain.p2p.effects

import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.comm.protocol.routing.Packet
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.shared._

trait PacketHandler[F[_]] {
  def handlePacket(peer: PeerNode, packet: Packet): F[Unit]
}

object PacketHandler extends PacketHandlerInstances {
  implicit private val logSource: LogSource = LogSource(this.getClass)

  def apply[F[_]: PacketHandler]: PacketHandler[F] = implicitly[PacketHandler[F]]

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: PacketHandler[F]
  ): PacketHandler[T[F, ?]] =
    new PacketHandler[T[F, ?]] {
      def handlePacket(peer: PeerNode, packet: Packet): T[F, Unit] =
        C.handlePacket(peer, packet).liftM[T]
    }

  def pf[F[_]](pfForPeer: (PeerNode) => PartialFunction[Packet, F[Unit]])(
      implicit ev1: Applicative[F],
      ev2: Log[F],
      errorHandler: ApplicativeError_[F, CommError]
  ): PacketHandler[F] =
    new PacketHandler[F] {
      def handlePacket(peer: PeerNode, packet: Packet): F[Unit] = {
        val errorMsg = s"Unable to handle packet $packet"
        val pf       = pfForPeer(peer)
        if (pf.isDefinedAt(packet)) pf(packet)
        else
          Log[F].error(errorMsg) *> errorHandler
            .raiseError(CommError.unknownProtocol(errorMsg))
      }
    }

  class NOPPacketHandler[F[_]: Applicative] extends PacketHandler[F] {
    def handlePacket(peer: PeerNode, packet: Packet): F[Unit] = ().pure[F]
  }

}

sealed abstract class PacketHandlerInstances {
  implicit def eitherTPacketHandler[E, F[_]: Monad: PacketHandler[?[_]]]
      : PacketHandler[EitherT[F, E, ?]] =
    PacketHandler.forTrans[F, EitherT[?[_], E, ?]]
}
