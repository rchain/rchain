package coop.rchain.comm.discovery

import cats.Monad
import cats.data.EitherT

import coop.rchain.comm.{NodeIdentifier, PeerNode}

trait NodeDiscovery[F[_]] {
  def discover: F[Unit]
  def peers: F[Seq[PeerNode]]
}

object NodeDiscovery extends NodeDiscoveryInstances {
  def apply[F[_]](implicit L: NodeDiscovery[F]): NodeDiscovery[F] = L
}

sealed abstract class NodeDiscoveryInstances {

  implicit def eitherTNodeDiscovery[E, F[_]: Monad: NodeDiscovery]
      : NodeDiscovery[EitherT[F, E, ?]] =
    new NodeDiscovery[EitherT[F, E, ?]] {

      def discover: EitherT[F, E, Unit] =
        EitherT.liftF(NodeDiscovery[F].discover)

      def peers: EitherT[F, E, Seq[PeerNode]] =
        EitherT.liftF(NodeDiscovery[F].peers)
    }

  def kademlia[F[_]: Monad: KademliaStore: KademliaRPC](id: NodeIdentifier): NodeDiscovery[F] =
    new NodeDiscovery[F] {
      def discover: F[Unit]       = KademliaNodeDiscovery.discover(id)
      def peers: F[Seq[PeerNode]] = KademliaNodeDiscovery.peers
    }
}
