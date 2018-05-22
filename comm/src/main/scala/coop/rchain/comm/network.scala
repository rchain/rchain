package coop.rchain.comm

import java.net.{SocketAddress, SocketTimeoutException}
import scala.collection.concurrent
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.comm.protocol.routing.Header
import coop.rchain.kademlia.PeerTable
import coop.rchain.metrics.Metrics
import com.typesafe.scalalogging.Logger
import scala.collection.mutable
import scala.util.{Failure, Success}
import CommError._
import cats._, cats.data._, cats.implicits._
import coop.rchain.p2p.effects._
import coop.rchain.catscontrib._, Catscontrib._
import cats._, cats.data._, cats.implicits._
import scala.util.Try

/**
  * Implements the lower levels of the network protocol.
  */
class UnicastNetwork(peer: PeerNode) {

  val logger = Logger("network-overlay")

  val id: NodeIdentifier = peer.id
  val endpoint: Endpoint = peer.endpoint

  @SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass")) // SI-4440
  case class PendingKey(remote: Seq[Byte], timestamp: Long, seq: Long)

  val pending =
    new concurrent.TrieMap[PendingKey, Promise[Either[CommError, ProtocolMessage]]]

  val unsafeRoundTrip: (ProtocolMessage, ProtocolNode) => CommErr[ProtocolMessage] =
    (pm, pn) => roundTrip[Id](pm, pn)

  val local = ProtocolNode(peer, unsafeRoundTrip)
  val comm  = new UnicastComm(local)
  val table = PeerTable(local)

  def receiver[F[_]: Monad: Capture: Log: Time: Metrics]: F[Option[ProtocolMessage]] =
    Capture[F].capture(comm.recv) >>= (result => {
      result match {
        case Right((sock, res)) =>
          val msgErr: Either[CommError, ProtocolMessage] = ProtocolMessage.parse(res)
          val dispatched: F[Either[CommError, Option[ProtocolMessage]]] =
            msgErr.traverse(msg => dispatch[F](sock, msg))
          dispatched.flatMap(e =>
            e match {
              case Left(commErr) => Log[F].error("Error in receiver: " + commErr).as(None)
              case Right(r)      => r.pure[F]
          })
        // TODO flatten that pattern match
        case Left(err: CommError) =>
          err match {
            case DatagramException(_: SocketTimeoutException) => none.pure[F]
            // TODO These next ones may ding a node's reputation; just
            // printing for now.
            case DatagramSizeError(sz) => Log[F].warn(s"bad datagram size $sz").as(none)
            case DatagramFramingError(ex) =>
              Log[F].error(s"datgram framing error, ex: $ex").as(none)
            case DatagramException(ex) => Log[F].error(s"datgram error, ex: $ex").as(none)
            case _                     => none.pure[F]
          }
      }
    })

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
  def findMorePeers(limit: Int): Seq[PeerNode] = {
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

  def pumpSender(sock: SocketAddress, sndr: PeerNode): PeerNode = sock match {
    case (s: java.net.InetSocketAddress) =>
      sndr.withUdpSocket(s)
    case _ => sndr
  }

  def dispatch[F[_]: Monad: Capture: Log: Time: Metrics](
      sock: SocketAddress,
      msg: ProtocolMessage): F[Option[ProtocolMessage]] = {

    val dispatchForSender: Option[F[Option[ProtocolMessage]]] = msg.sender.map { sndr =>
      val sender = pumpSender(sock, sndr)

      // Update sender's last-seen time, adding it if there are no higher-level protocols.
      for {
        ti <- Time[F].nanoTime
        _  <- Capture[F].capture(table.observe(ProtocolNode(sender, local, unsafeRoundTrip), true))
        tf <- Time[F].nanoTime
        // Capture µs timing for roundtrips
        _ <- Metrics[F].record("network-roundtrip-micros", (tf - ti) / 1000)
        ret <- msg match {
                case ping @ PingMessage(_, _)     => handlePing[F](sender, ping).as(None)
                case lookup @ LookupMessage(_, _) => handleLookup[F](sender, lookup).as(None)
                case disconnect @ DisconnectMessage(_, _) =>
                  handleDisconnect[F](sender, disconnect).as(None)
                case resp: ProtocolResponse => handleResponse[F](sock, sender, resp)
                case _                      => Some(msg).pure[F]
              }
      } yield ret

    }

    dispatchForSender.getOrElse(Log[F].error("Tried to dispatch message without a sender").as(None))
  }

  def add(peer: PeerNode): Unit =
    table.observe(ProtocolNode(peer, local, unsafeRoundTrip), add = true)

  /*
   * Handle a response to a message. If this message isn't one we were
   * expecting, propagate it to the next dispatcher.
   */
  private def handleResponse[F[_]: Monad: Capture: Log: Time: Metrics](
      sock: SocketAddress,
      sender: PeerNode,
      msg: ProtocolResponse): F[Option[ProtocolMessage]] = {
    val handleWithHeader: Option[F[Option[ProtocolMessage]]] = for {
      returnHeader <- msg.returnHeader
    } yield {
      for {
        result <- Capture[F].capture(
                   pending.get(PendingKey(sender.key, returnHeader.timestamp, returnHeader.seq)))
        ret <- result match {
                case Some(promise) => Capture[F].capture(promise.success(Right(msg))).as(None)
                case None          => Some(msg).pure[F]
              }

      } yield ret

    }
    handleWithHeader.getOrElse(
      Log[F].error("Could not handle response, header or sender not available").as(None))
  }

  private def handlePing[F[_]: Functor: Capture: Log: Metrics](sender: PeerNode,
                                                               ping: PingMessage): F[Unit] =
    ping
      .response(local)
      .map { pong =>
        Metrics[F].incrementCounter("ping-recv-count")
        Capture[F].capture(comm.send(pong.toByteSeq, sender)).void
      }
      .getOrElse(Log[F].error(s"Response was not available for ping: $ping"))

  /**
    * Validate incoming LOOKUP message and return an answering
    * LOOKUP_RESPONSE.
    */
  private def handleLookup[F[_]: Monad: Capture: Log: Metrics](sender: PeerNode,
                                                               lookup: LookupMessage): F[Unit] =
    (for {
      id   <- lookup.lookupId
      resp <- lookup.response(local, table.lookup(id))
    } yield {
      Metrics[F].incrementCounter("lookup-recv-count")
      Capture[F].capture(comm.send(resp.toByteSeq, sender)).void
    }).getOrElse(Log[F].error(s"lookupId or resp not available for lookup: $lookup"))

  /**
    * Remove sending peer from table.
    */
  private def handleDisconnect[F[_]: Monad: Capture: Log: Metrics](
      sender: PeerNode,
      disconnect: DisconnectMessage): F[Unit] =
    for {
      _ <- Log[F].info(s"Forgetting about $sender.")
      _ <- Capture[F].capture(table.remove(sender.key))
      _ <- Metrics[F].incrementCounter("disconnect-recv-count")
      _ <- Metrics[F].decrementGauge("peers")
    } yield ()

  /**
    * Broadcast a message to all peers in the Kademlia table.
    */
  def broadcast(msg: ProtocolMessage): Seq[Either[CommError, Unit]] = {
    val bytes = msg.toByteSeq
    table.peers.par.map { p =>
      comm.send(bytes, p)
    }.toList
  }

  /**
    * Send a message to a single, identified peer and return any
    * response that arrives within `timeout`. Receipt of messages is
    * asynchronous in this protocol, so create a promise and await its
    * completion.
    *
    * This method should be called in its own thread.
    */
  def roundTrip[G[_]: Capture: Monad](
      msg: ProtocolMessage,
      remote: ProtocolNode,
      timeout: Duration = Duration(500, MILLISECONDS)): G[CommErr[ProtocolMessage]] = {

    def send(header: Header): CommErrT[G, ProtocolMessage] = {
      val sendIt: G[CommErr[ProtocolMessage]] = for {
        promise <- Capture[G].capture(Promise[Either[CommError, ProtocolMessage]])
        bytes   = msg.toByteSeq
        pend    = PendingKey(remote.key, header.timestamp, header.seq)
        result <- Capture[G].capture {
                   pending.put(pend, promise)
                   comm.send(bytes, remote)
                   Try(Await.result(promise.future, timeout)).toEither
                     .leftMap(protocolException)
                     .flatten
                 }
        _ <- Capture[G].capture(pending.remove(pend))
      } yield result
      EitherT(sendIt)
    }

    def fetchHeader(maybeHeader: Option[Header]): CommErrT[G, Header] =
      maybeHeader.fold[CommErrT[G, Header]](
        EitherT(unknownProtocol("malformed message").asLeft[Header].pure[G]))(
        _.pure[CommErrT[G, ?]])

    (for {
      header     <- fetchHeader(msg.header)
      messageErr <- send(header)
    } yield messageErr).value

  }

  override def toString = s"#{Network $local ${local.endpoint.udpSocketAddress}}"
}
