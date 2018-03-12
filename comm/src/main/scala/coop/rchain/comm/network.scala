package coop.rchain.comm

import java.net.{SocketAddress, SocketTimeoutException}
import scala.collection.concurrent
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.kademlia.PeerTable
import coop.rchain.comm.protocol.routing.Header
import com.typesafe.scalalogging.Logger
import scala.collection.mutable
import scala.util.{Failure, Success}
import CommError._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import cats._, cats.data._, cats.implicits._
import scala.util.Try

/**
  * Implements the lower levels of the network protocol.
  */
final case class UnicastNetwork(peer: PeerNode,
                                next: Option[ProtocolDispatcher[SocketAddress]] = None)
    extends ProtocolHandler
    with ProtocolDispatcher[SocketAddress] {

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

  def receiver[
      F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption: Kvs[?[_],
                                                                               PeerNode,
                                                                               Array[Byte]]]
    : F[Unit] =
    for {
      result <- Capture[F].capture(comm.recv)
      _ <- result match {
            case Right((sock, res)) =>
              ProtocolMessage.parse(res).traverse { msg =>
                dispatch[F](sock, msg)
              }
            // TODO flatten that pattern match
            case Left(err: CommError) =>
              err match {
                case DatagramException(ex: SocketTimeoutException) => ().pure[F]
                // TODO These next ones may ding a node's reputation; just
                // printing for now.
                case DatagramSizeError(sz) => Log[F].warn(s"bad datagram size $sz")
                case DatagramFramingError(ex) =>
                  Log[F].error(s"datgram framing error, ex: $ex")
                case DatagramException(ex) => Log[F].error(s"datgram error, ex: $ex")
                case _                     => ().pure[F]
              }

          }

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
  def findMorePeers(limit: Int): Seq[PeerNode] = {
    var currentSet = table.peers.toSet
    val potentials = mutable.Set[PeerNode]()
    if (currentSet.size > 0) {
      val dists = table.sparseness()
      var i     = 0
      while (currentSet.size > 0 && potentials.size < limit && i < dists.size) {
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
              !potentials.contains(r) && r.id.key != id.key && table.find(r.id.key) == None)
          case _ => ()
        }
        currentSet -= currentSet.head
        i += 1
      }
    }
    potentials.toSeq
  }

  def dispatch[
      F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption: Kvs[?[_],
                                                                               PeerNode,
                                                                               Array[Byte]]](
      sock: SocketAddress,
      msg: ProtocolMessage): F[Unit] = {

    val dispatchForSender: Option[F[Unit]] = msg.sender.map { sndr =>
      val sender =
        sock match {
          case (s: java.net.InetSocketAddress) =>
            sndr.withUdpSocket(s)
          case _ => sndr
        }

      // Update sender's last-seen time, adding it if there are no higher-level protocols.
      for {
        ti <- Time[F].nanoTime
        _ <- Capture[F].capture(
              table.observe(ProtocolNode(sender, local, unsafeRoundTrip), next == None))
        tf <- Time[F].nanoTime
        // Capture µs timing for roundtrips
        _ <- Metrics[F].record("network-roundtrip-micros", (tf - ti) / 1000)
        _ <- msg match {
              case ping @ PingMessage(_, _)             => handlePing[F](sender, ping)
              case lookup @ LookupMessage(_, _)         => handleLookup[F](sender, lookup)
              case disconnect @ DisconnectMessage(_, _) => handleDisconnect[F](sender, disconnect)
              case resp: ProtocolResponse               => handleResponse[F](sock, sender, resp)
              case _                                    => next.traverse(d => d.dispatch[F](sock, msg)).void
            }
      } yield ()

    }

    dispatchForSender.getOrElse(Log[F].error("Tried to dispatch message without a sender"))
  }

  def add(peer: PeerNode): Unit =
    table.observe(ProtocolNode(peer, local, unsafeRoundTrip), true)

  /*
   * Handle a response to a message. If this message isn't one we were
   * expecting, propagate it to the next dispatcher.
   */
  private def handleResponse[
      F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption: Kvs[?[_],
                                                                               PeerNode,
                                                                               Array[Byte]]](
      sock: SocketAddress,
      sender: PeerNode,
      msg: ProtocolResponse): F[Unit] = {
    val handleWithHeader: Option[F[Unit]] = msg.returnHeader.map { ret =>
      for {
        result <- Capture[F].capture(pending.get(PendingKey(sender.key, ret.timestamp, ret.seq)))
        _ <- result match {
              case Some(promise) => Capture[F].capture(promise.success(Right(msg)))
              case None          => next.traverse(_.dispatch[F](sock, msg)).void
            }

      } yield ()

    }
    handleWithHeader.getOrElse(Log[F].error("Could not handle response, header not available"))
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
      _ <- Metrics[F].incrementCounter("peers", -1L)
    } yield ()

  /**
    * Broadcast a message to all peers in the Kademlia table.
    */
  override def broadcast(msg: ProtocolMessage): Seq[Either[CommError, Unit]] = {
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
  override def roundTrip[F[_]: Capture: Monad](
      msg: ProtocolMessage,
      remote: ProtocolNode,
      timeout: Duration = Duration(500, MILLISECONDS)): F[CommErr[ProtocolMessage]] = {

    def send(header: Header): CommErrT[F, ProtocolMessage] = {
      val sendIt: F[CommErr[ProtocolMessage]] = for {
        promise <- Capture[F].capture(Promise[Either[CommError, ProtocolMessage]])
        bytes   = msg.toByteSeq
        pend    = PendingKey(remote.key, header.timestamp, header.seq)
        result <- Capture[F].capture {
                   pending.put(pend, promise)
                   comm.send(bytes, remote)
                   Try(Await.result(promise.future, timeout)).toEither
                     .leftMap(protocolException)
                     .flatten
                 }
        _ <- Capture[F].capture(pending.remove(pend))
      } yield result
      EitherT(sendIt)
    }

    def fetchHeader(maybeHeader: Option[Header]): CommErrT[F, Header] =
      maybeHeader.fold[CommErrT[F, Header]](
        EitherT(unknownProtocol("malformed message").asLeft[Header].pure[F]))(
        _.pure[CommErrT[F, ?]])

    (for {
      header     <- fetchHeader(msg.header)
      messageErr <- send(header)
    } yield messageErr).value

  }

  override def toString = s"#{Network $local ${local.endpoint.udpSocketAddress}}"
}
