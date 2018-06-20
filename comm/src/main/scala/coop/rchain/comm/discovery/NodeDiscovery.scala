package coop.rchain.comm.discovery

import coop.rchain.comm.transport._

import cats.Monad
import cats.data.EitherT
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.{MonadTrans, _}
import coop.rchain.comm.{PeerNode, ProtocolMessage}

trait NodeDiscovery[F[_]] {
  def addNode(node: PeerNode): F[Unit]
  def findMorePeers(limit: Int): F[Seq[PeerNode]]
  def peers: F[Seq[PeerNode]]
  def handleCommunications: ProtocolMessage => F[CommunicationResponse]
}

object NodeDiscovery extends NodeDiscoveryInstances {
  def apply[F[_]](implicit L: NodeDiscovery[F]): NodeDiscovery[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: NodeDiscovery[F]): NodeDiscovery[T[F, ?]] =
    new NodeDiscovery[T[F, ?]] {
      def addNode(node: PeerNode): T[F, Unit]            = C.addNode(node).liftM[T]
      def findMorePeers(limit: Int): T[F, Seq[PeerNode]] = C.findMorePeers(limit).liftM[T]
      def peers: T[F, Seq[PeerNode]]                     = C.peers.liftM[T]
      def handleCommunications: ProtocolMessage => T[F, CommunicationResponse] =
        pm => C.handleCommunications(pm).liftM[T]
    }
}

sealed abstract class NodeDiscoveryInstances {
  implicit def eitherTNodeDiscovery[E, F[_]: Monad: NodeDiscovery[?[_]]]
    : NodeDiscovery[EitherT[F, E, ?]] =
    NodeDiscovery.forTrans[F, EitherT[?[_], E, ?]]
}
