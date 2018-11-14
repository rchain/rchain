package coop.rchain.comm.discovery

import scala.collection.mutable
import scala.concurrent.duration._

import cats._
import cats.implicits._

import coop.rchain.catscontrib._
import Catscontrib._
import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import coop.rchain.shared._

object KademliaNodeDiscovery {
  def create[F[_]: Monad: Capture: Log: Time: Metrics: KademliaRPC](
      id: NodeIdentifier,
      defaultTimeout: FiniteDuration
  )(init: Option[PeerNode]): F[KademliaNodeDiscovery[F]] =
    for {
      knd <- new KademliaNodeDiscovery[F](id, defaultTimeout).pure[F]
      _   <- init.fold(().pure[F])(p => knd.addNode(p))
    } yield knd

}

private[discovery] class KademliaNodeDiscovery[F[_]: Monad: Capture: Log: Time: Metrics: KademliaRPC](
    id: NodeIdentifier,
    timeout: FiniteDuration
) extends NodeDiscovery[F] {

  private val table = PeerTable[PeerNode](id.key)

  // TODO inline usage
  private[discovery] def addNode(peer: PeerNode): F[Unit] =
    for {
      _ <- table.updateLastSeen[F](peer)
      _ <- Metrics[F].setGauge("kademlia-peers", table.peers.length.toLong)
    } yield ()

  private def pingHandler(peer: PeerNode): F[Unit] =
    addNode(peer) *> Metrics[F].incrementCounter("ping-recv-count")

  private def lookupHandler(peer: PeerNode, id: Array[Byte]): F[Seq[PeerNode]] =
    for {
      peers <- Capture[F].capture(table.lookup(id))
      _     <- Metrics[F].incrementCounter("lookup-recv-count")
      _     <- addNode(peer)
    } yield peers

  def discover: F[Unit] = {

    val initRPC = KademliaRPC[F].receive(pingHandler, lookupHandler)

    val findNewAndAdd = for {
      _     <- Time[F].sleep(9.seconds)
      peers <- findMorePeers(10).map(_.toList)
      _     <- peers.traverse(addNode)
    } yield ()

    initRPC *> findNewAndAdd.forever
  }

  /**
    * Return up to `limit` candidate peers.
    *
    * Curently, this function determines the distances in the table that are
    * least populated and searches for more peers to fill those. It asks one
    * node for peers at one distance, then moves on to the next node and
    * distance. The queried nodes are not in any particular order. For now, this
    * function should be called with a relatively small `limit` parameter like
    * 10 to avoid making too many unproductive networking calls.
    */
  private def findMorePeers(limit: Int): F[Seq[PeerNode]] = {
    val dists = table.sparseness().toArray

    def find(peerSet: Set[PeerNode], potentials: Set[PeerNode], i: Int): F[Seq[PeerNode]] =
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
        KademliaRPC[F]
          .lookup(target, peerSet.head)
          .map { results =>
            potentials ++ results.filter(
              r =>
                !potentials.contains(r)
                  && r.id.key != id.key
                  && table.find(r.id.key).isEmpty
            )
          } >>= (find(peerSet.tail, _, i + 1))
      } else {
        potentials.toSeq.pure[F]
      }

    find(table.peers.toSet, Set(), 0)
  }

  def peers: F[Seq[PeerNode]] = Capture[F].capture(table.peers)

}
