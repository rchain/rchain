package coop.rchain.node.effects

import scala.collection.mutable
import scala.concurrent.duration._
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics
import coop.rchain.kademlia.PeerTable
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import CommunicationResponse._

class TLNodeDiscovery[F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: Ping](src: PeerNode)
    extends NodeDiscovery[F] {

  private val table = PeerTable(src)

  private def updateLastSeen(peer: PeerNode): F[Unit] =
    table.observe[F](peer, add = true)

  private val id: NodeIdentifier = src.id

  def addNode(peer: PeerNode): F[Unit] =
    for {
      _ <- updateLastSeen(peer)
      _ <- Metrics[F].setGauge("peers", table.peers.length.toLong)
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
  def findMorePeers(limit: Int): F[Seq[PeerNode]] = {
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
        lookup(target, peerSet.head)
          .map { results =>
            potentials ++ results.filter(
              r =>
                !potentials.contains(r)
                  && r.id.key != id.key
                  && table.find(r.id.key).isEmpty)
          } >>= (find(peerSet.tail, _, i + 1))
      } else {
        potentials.toSeq.pure[F]
      }

    find(table.peers.toSet, Set(), 0)
  }

  def peers: F[Seq[PeerNode]] = Capture[F].capture(table.peers)

  def handleCommunications: ProtocolMessage => F[CommunicationResponse] =
    pm =>
      pm.sender.fold(notHandled.pure[F]) { sender =>
        updateLastSeen(sender) >>= kp(pm match {
          case ping @ PingMessage(_, _)             => handlePing(sender, ping)
          case lookup @ LookupMessage(_, _)         => handleLookup(sender, lookup)
          case disconnect @ DisconnectMessage(_, _) => handleDisconnect(sender, disconnect)
          case _                                    => notHandled.pure[F]
        })
    }

  private def handlePing(sender: PeerNode, ping: PingMessage): F[CommunicationResponse] =
    ping
      .response(src)
      .traverse { pong =>
        Metrics[F].incrementCounter("ping-recv-count").as(pong)
      }
      .map(_.fold(notHandled)(m => handledWithMessage(m)))

  /**
    * Validate incoming LOOKUP message and return an answering
    * LOOKUP_RESPONSE.
    */
  private def handleLookup(sender: PeerNode, lookup: LookupMessage): F[CommunicationResponse] =
    (for {
      id   <- lookup.lookupId
      resp <- lookup.response(src, table.lookup(id))
    } yield {
      Metrics[F].incrementCounter("lookup-recv-count").as(resp)
    }).sequence.map(_.fold(notHandled)(m => handledWithMessage(m)))

  /**
    * Remove sending peer from table.
    */
  private def handleDisconnect(sender: PeerNode,
                               disconnect: DisconnectMessage): F[CommunicationResponse] =
    for {
      _ <- Log[F].info(s"Forgetting about $sender.")
      _ <- Capture[F].capture(table.remove(sender.key))
      _ <- Metrics[F].incrementCounter("disconnect-recv-count")
      _ <- Metrics[F].setGauge("peers", table.peers.length.toLong)
    } yield handledWitoutMessage

  private def lookup(key: Seq[Byte], remoteNode: PeerNode): F[Seq[PeerNode]] =
    for {
      _   <- Metrics[F].incrementCounter("protocol-lookup-send")
      req = LookupMessage(ProtocolMessage.lookup(src, key), System.currentTimeMillis)
      r <- TransportLayer[F]
            .roundTrip(req, remoteNode, 500.milliseconds)
            .map(_.toOption
              .map {
                case LookupResponseMessage(proto, _) =>
                  proto.message.lookupResponse
                    .map(_.nodes.map(ProtocolMessage.toPeerNode))
                    .getOrElse(Seq())
                case _ => Seq()
              }
              .getOrElse(Seq()))
    } yield r
}
