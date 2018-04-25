package coop.rchain.p2p.effects

import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats.Monad
import cats.data.EitherT

import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolMessage, ProtocolNode}

trait TransportLayer[F[_]] {
  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration): F[CommErr[ProtocolMessage]]
  def local: F[ProtocolNode]
  def commSend(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]]
  def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]]
}

object TransportLayer extends TransportLayerInstances {
  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: TransportLayer[F]): TransportLayer[T[F, ?]] =
    new TransportLayer[T[F, ?]] {
      def roundTrip(msg: ProtocolMessage,
                    remote: ProtocolNode,
                    timeout: Duration): T[F, CommErr[ProtocolMessage]] =
        C.roundTrip(msg, remote, timeout).liftM[T]
      def local: T[F, ProtocolNode] = C.local.liftM[T]
      def commSend(msg: ProtocolMessage, p: PeerNode): T[F, CommErr[Unit]] =
        C.commSend(msg, p).liftM[T]
      def broadcast(msg: ProtocolMessage): T[F, Seq[CommErr[Unit]]] = C.broadcast(msg).liftM[T]
    }
}

sealed abstract class TransportLayerInstances {
  implicit def eitherTTransportLayer[E, F[_]: Monad: TransportLayer[?[_]]]
    : TransportLayer[EitherT[F, E, ?]] =
    TransportLayer.forTrans[F, EitherT[?[_], E, ?]]
}
