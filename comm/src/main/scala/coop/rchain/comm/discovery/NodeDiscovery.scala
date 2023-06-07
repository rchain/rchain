package coop.rchain.comm.discovery

import cats.Monad
import coop.rchain.comm.{NodeIdentifier, PeerNode}

trait NodeDiscovery[F[_]] {
  def discover: F[Unit]
  def peers: F[Seq[PeerNode]]
}

object NodeDiscovery extends NodeDiscoveryInstances {
  def apply[F[_]](implicit L: NodeDiscovery[F]): NodeDiscovery[F] = L
}

sealed abstract class NodeDiscoveryInstances {

  def kademlia[F[_]: Monad: KademliaStore: KademliaRPC](id: NodeIdentifier): NodeDiscovery[F] =
    new NodeDiscovery[F] {
      def discover: F[Unit]       = KademliaNodeDiscovery.discover(id)
      def peers: F[Seq[PeerNode]] = KademliaNodeDiscovery.peers
    }
}
