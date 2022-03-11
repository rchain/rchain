package coop.rchain.comm.discovery

import scala.collection.mutable
import scala.util.Random

import cats._
import cats.syntax.all._

import coop.rchain.comm._

object KademliaNodeDiscovery {

  /**
    * Return up to `limit` candidate peers.
    *
    * Currently, this function determines the distances in the table that are
    * least populated and searches for more peers to fill those. It asks one
    * node for peers at one distance, then moves on to the next node and
    * distance. The queried nodes are not in any particular order. For now, this
    * function should be called with a relatively small `limit` parameter like
    * 10 to avoid making too many unproductive network calls.
    */
  def discover[F[_]: Monad: KademliaStore: KademliaRPC](id: NodeIdentifier): F[Unit] = {
    def find(
        limit: Int,
        dists: Array[Int],
        peerSet: List[PeerNode],
        potentials: Set[PeerNode],
        i: Int
    ): F[List[PeerNode]] =
      if (peerSet.nonEmpty && potentials.size < limit && i < dists.length) {
        val dist = dists(i)
        /*
         * The general idea is to ask a peer for its peers around a certain
         * distance from our own key. So, construct a key that first differs
         * from ours at bit position dist.
         */
        val target       = id.key.to[mutable.ArrayBuffer] // Our key
        val byteIndex    = dist / 8
        val differentBit = 1 << (dist % 8)
        target(byteIndex) = (target(byteIndex) ^ differentBit).toByte // A key at a distance dist from me

        KademliaRPC[F].lookup(target, peerSet.head) >>= (filter(_, potentials)) >>= (
            ps => find(limit, dists, peerSet.tail, potentials ++ ps, i + 1)
        )
      } else {
        potentials.toList.pure[F]
      }

    def filter(peers: Seq[PeerNode], potentials: Set[PeerNode]): F[List[PeerNode]] =
      peers.toList
        .filterNot(p => potentials.contains(p) || p.id.key == id.key)
        .filterA(p => KademliaStore[F].find(p.id.key).map(_.isEmpty))

    for {
      peers  <- KademliaStore[F].peers
      dists  <- KademliaStore[F].sparseness
      result <- find(10, dists.toArray, Random.shuffle(peers.toList), Set(), 0)
      _      <- result.traverse(KademliaStore[F].updateLastSeen)
    } yield ()
  }

  def peers[F[_]: KademliaStore]: F[Seq[PeerNode]] = KademliaStore[F].peers
}
