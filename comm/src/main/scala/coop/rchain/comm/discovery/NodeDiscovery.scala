package coop.rchain.comm.discovery

import coop.rchain.comm.transport._

import cats.Monad
import cats.data.EitherT
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.{MonadTrans, _}
import coop.rchain.comm.{CommError, PeerNode}, CommError.CommErr
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.protocol.routing._

trait NodeDiscovery[F[_]] {
  def discover: F[Unit]
  def peers: F[Seq[PeerNode]]
}

object NodeDiscovery extends NodeDiscoveryInstances {
  def apply[F[_]](implicit L: NodeDiscovery[F]): NodeDiscovery[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: NodeDiscovery[F]
  ): NodeDiscovery[T[F, ?]] =
    new NodeDiscovery[T[F, ?]] {
      def discover: T[F, Unit]       = C.discover.liftM[T]
      def peers: T[F, Seq[PeerNode]] = C.peers.liftM[T]
    }
}

sealed abstract class NodeDiscoveryInstances {
  implicit def eitherTNodeDiscovery[E, F[_]: Monad: NodeDiscovery[?[_]]]
    : NodeDiscovery[EitherT[F, E, ?]] =
    NodeDiscovery.forTrans[F, EitherT[?[_], E, ?]]
}
