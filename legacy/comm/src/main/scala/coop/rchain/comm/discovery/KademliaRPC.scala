package coop.rchain.comm.discovery

import coop.rchain.comm.PeerNode

trait KademliaRPC[F[_]] {
  def ping(node: PeerNode): F[Boolean]
  def lookup(key: Seq[Byte], peer: PeerNode): F[Seq[PeerNode]]
}

object KademliaRPC {
  def apply[F[_]](implicit P: KademliaRPC[F]): KademliaRPC[F] = P
}
