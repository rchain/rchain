package coop.rchain.node.effects

import scala.collection.mutable
import scala.util.{Failure, Success}
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics
import coop.rchain.kademlia.PeerTable
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._

class TLNodeDiscovery[F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer](src: PeerNode)(
    toProtocolNode: ((PeerNode, Option[ProtocolNode])) => ProtocolNode)
    extends NodeDiscovery[F] {

  private val local = toProtocolNode(src, none)
  private val table = PeerTable(local)

  private def updateLastSeen(peer: PeerNode): F[Unit] =
    Capture[F].capture(table.observe(toProtocolNode(peer, local.some), add = true))

  private val id: NodeIdentifier = src.id
  private val endpoint: Endpoint = src.endpoint

  def addNode(peer: PeerNode): F[Unit] =
    for {
      _ <- updateLastSeen(peer)
      _ <- Metrics[F].incrementGauge("peers")
    } yield ()

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
  def findMorePeers(limit: Int): F[Seq[PeerNode]] =
    Capture[F].capture {
      var currentSet = table.peers.toSet
      val potentials = mutable.Set[PeerNode]()
      if (currentSet.nonEmpty) {
        val dists = table.sparseness()
        var i     = 0
        while (currentSet.nonEmpty && potentials.size < limit && i < dists.size) {
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
          currentSet.head.lookup(target) match {
            case Success(results) =>
              potentials ++= results.filter(r =>
                !potentials.contains(r) && r.id.key != id.key && table.find(r.id.key).isEmpty)
            case _ => ()
          }
          currentSet -= currentSet.head
          i += 1
        }
      }
      potentials.toSeq
    }

  def peers: F[Seq[PeerNode]] = Capture[F].capture(table.peers)

  def handleCommunications: ProtocolMessage => F[Option[ProtocolMessage]] =
    pm =>
      pm.sender.fold(none[ProtocolMessage].pure[F]) { sender =>
        updateLastSeen(sender) >>= kp(pm match {
          case ping @ PingMessage(_, _)             => handlePing(sender, ping)
          case lookup @ LookupMessage(_, _)         => handleLookup(sender, lookup)
          case disconnect @ DisconnectMessage(_, _) => handleDisconnect(sender, disconnect)
          case _                                    => none[ProtocolMessage].pure[F]
        })
    }

  private def handlePing(sender: PeerNode, ping: PingMessage): F[Option[ProtocolMessage]] =
    ping
      .response(local)
      .traverse { pong =>
        Metrics[F].incrementCounter("ping-recv-count").as(pong)
      }

  /**
    * Validate incoming LOOKUP message and return an answering
    * LOOKUP_RESPONSE.
    */
  private def handleLookup(sender: PeerNode, lookup: LookupMessage): F[Option[ProtocolMessage]] =
    (for {
      id   <- lookup.lookupId
      resp <- lookup.response(local, table.lookup(id))
    } yield {
      Metrics[F].incrementCounter("lookup-recv-count").as(resp)
    }).sequence

  /**
    * Remove sending peer from table.
    */
  private def handleDisconnect(sender: PeerNode,
                               disconnect: DisconnectMessage): F[Option[ProtocolMessage]] =
    for {
      _ <- Log[F].info(s"Forgetting about $sender.")
      _ <- Capture[F].capture(table.remove(sender.key))
      _ <- Metrics[F].incrementCounter("disconnect-recv-count")
      _ <- Metrics[F].decrementGauge("peers")
    } yield none[ProtocolMessage]

}
