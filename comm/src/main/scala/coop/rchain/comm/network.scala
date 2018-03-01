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
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import cats._, cats.data._, cats.implicits._

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

  val local = new ProtocolNode(id, endpoint, this)
  val comm  = new UnicastComm(local)
  val table = PeerTable(local)

  def receiver[F[_]: Monad: Capture: Log]: F[Unit] =
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

  def dispatch[F[_]: Monad: Capture: Log](sock: SocketAddress, msg: ProtocolMessage): F[Unit] = {

    val dispatchForSender: Option[F[Unit]] = msg.sender.map { sndr =>
      val sender =
        sock match {
          case (s: java.net.InetSocketAddress) =>
            sndr.withUdpSocket(s)
          case _ => sndr
        }

      // Update sender's last-seen time, adding it if there are no higher-level protocols.
      for {
        _ <- Capture[F].capture(table.observe(new ProtocolNode(sender, this), next == None))
        _ <- msg match {
              case ping @ PingMessage(_, _)     => Capture[F].capture(handlePing(sender, ping))
              case lookup @ LookupMessage(_, _) => Capture[F].capture(handleLookup(sender, lookup))
              case disconnect @ DisconnectMessage(_, _) =>
                Capture[F].capture(handleDisconnect(sender, disconnect))
              case resp: ProtocolResponse => Capture[F].capture(handleResponse(sock, sender, resp))
              case _                      => next.traverse(d => d.dispatch[F](sock, msg)).void
            }
      } yield ()

    }

    dispatchForSender.getOrElse(Log[F].error("Tried to dispatch message without a sender"))
  }

  def add(peer: PeerNode): Unit = table.observe(new ProtocolNode(peer, this), true)

  /*
   * Handle a response to a message. If this message isn't one we were
   * expecting, propagate it to the next dispatcher.
   */
  private def handleResponse(sock: SocketAddress, sender: PeerNode, msg: ProtocolResponse): Unit =
    for {
      ret <- msg.returnHeader
    } {
      pending.get(PendingKey(sender.key, ret.timestamp, ret.seq)) match {
        case Some(promise) => promise.success(Right(msg))
        // case None          => next.foreach(_.dispatch(sock, msg))
      }
    }

  /**
    * Validate incoming PING and send responding PONG.
    */
  private def handlePing(sender: PeerNode, ping: PingMessage): Unit =
    for {
      pong <- ping.response(local)
    } {
      comm.send(pong.toByteSeq, sender)
    }

  /**
    * Validate incoming LOOKUP message and return an answering
    * LOOKUP_RESPONSE.
    */
  private def handleLookup(sender: PeerNode, lookup: LookupMessage): Unit =
    for {
      id   <- lookup.lookupId
      resp <- lookup.response(local, table.lookup(id))
    } {
      comm.send(resp.toByteSeq, sender)
    }

  /**
    * Remove sending peer from table.
    */
  private def handleDisconnect(sender: PeerNode, disconnect: DisconnectMessage): Unit = {
    logger.info(s"Forgetting about $sender.")
    table.remove(sender.key)
  }

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
  override def roundTrip[F[_]: Capture: Monad](msg: ProtocolMessage,
                                               remote: ProtocolNode,
                                               timeout: Duration = Duration(500, MILLISECONDS))(
      implicit err: ApplicativeError_[F, CommError]): F[ProtocolMessage] = {

    def send(header: Header): F[ProtocolMessage] =
      Capture[F].capture {
        val bytes   = msg.toByteSeq
        val pend    = PendingKey(remote.key, header.timestamp, header.seq)
        val promise = Promise[Either[CommError, ProtocolMessage]]
        pending.put(pend, promise)
        // TODO used to be in try
        comm.send(bytes, remote)
        val result = Await.result(promise.future, timeout)
        // TODO used to by in finally
        pending.remove(pend)
        err.fromEither(result)
      }.flatten

    def fetchHeader(maybeHeader: Option[Header]): F[Header] =
      maybeHeader.fold[F[Header]](
        err.raiseError[Header](UnknownProtocolError("malformed message")))(_.pure[F])

    for {
      header  <- fetchHeader(msg.header)
      message <- send(header)
    } yield message

  }

  override def toString = s"#{Network $local ${local.endpoint.udpSocketAddress}}"
}
