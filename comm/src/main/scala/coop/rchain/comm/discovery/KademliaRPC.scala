package coop.rchain.comm.discovery

import scala.concurrent.duration.Duration

import cats._, cats.data._

import coop.rchain.catscontrib.{MonadTrans, _}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode

trait KademliaRPC[F[_]] {
  def ping(node: PeerNode): F[Boolean]
  def lookup(key: Seq[Byte], peer: PeerNode): F[Seq[PeerNode]]
}

object KademliaRPC extends KademliaRPCInstances {
  def apply[F[_]](implicit P: KademliaRPC[F]): KademliaRPC[F] = P

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit P: KademliaRPC[F]
  ): KademliaRPC[T[F, ?]] =
    new KademliaRPC[T[F, ?]] {
      def ping(node: PeerNode): T[F, Boolean]                         = P.ping(node).liftM[T]
      def lookup(key: Seq[Byte], peer: PeerNode): T[F, Seq[PeerNode]] = P.lookup(key, peer).liftM[T]
    }
}

sealed abstract class KademliaRPCInstances {
  implicit def eitherTKademliaRPC[E, F[_]: Monad: KademliaRPC[?[_]]]
    : KademliaRPC[EitherT[F, E, ?]] =
    KademliaRPC.forTrans[F, EitherT[?[_], E, ?]]
}
