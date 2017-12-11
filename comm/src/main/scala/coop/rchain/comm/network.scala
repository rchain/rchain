package coop.rchain.comm

import java.net.SocketTimeoutException
import scala.collection.concurrent
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.kademlia.PeerTable
import com.typesafe.scalalogging.Logger
import scala.collection.mutable
import scala.util.{Success, Failure}

/**
  * Implements the lower levels of the network protocol.
  */
case class UnicastNetwork(id: NodeIdentifier,
                          endpoint: Endpoint,
                          next: Option[ProtocolDispatcher] = None)
    extends ProtocolHandler
    with ProtocolDispatcher {

  val logger = Logger("network-overlay")

  def this(peer: PeerNode, next: Option[ProtocolDispatcher]) =
    this(peer.id, peer.endpoint, next)

  case class PendingKey(remote: Seq[Byte], timestamp: Long, seq: Long)

  val pending =
    new concurrent.TrieMap[PendingKey, Promise[Either[CommError, ProtocolMessage]]]

  val local = new ProtocolNode(id, endpoint, this)
  val comm = UnicastComm(local)
  val table = PeerTable(local)

  private val receiver = new Thread {
    override def run =
      while (true) {
        comm.recv match {
          case Right(res) =>
            for {
              msg <- ProtocolMessage.parse(res)
            } dispatch(msg)
          case Left(err: CommError) =>
            err match {
              case DatagramException(ex: SocketTimeoutException) => ()
              // These next ones may ding a node's reputation; just
              // printing for now.
              case err @ DatagramSizeError(sz)    => logger.warn(s"bad datagram size $sz")
              case err @ DatagramFramingError(ex) => ex.printStackTrace
              case err @ DatagramException(ex)    => ex.printStackTrace

              case _ => ()
            }
        }
      }
  }

  def findMorePeers(limit: Int): Seq[PeerNode] = {
    var currentSet = table.peers.toSet
    val potentials = mutable.Set[PeerNode]()
    if (currentSet.size > 0) {
      val dists = table.sparseness()
      var i = 0
      while (currentSet.size > 0 && potentials.size < limit && i < dists.size) {
        val dist = dists(i)
        val target = id.key.to[mutable.ArrayBuffer]
        val bno = dist/8
        val mask = 1 << (dist%8)
        target(bno) = (target(bno)^mask).toByte
        currentSet.head.lookup(target) match {
          case Success(results) =>
            potentials ++= results.filter(r => !potentials.contains(r) && r.id.key != id.key && table.find(r.id.key) == None)
          case _ => ()
        }
        currentSet -= currentSet.head
        i += 1
      }
    }
    potentials.toSeq
  }

  def dispatch(msg: ProtocolMessage): Unit =
    for {
      sender <- msg.sender
    } {
      // Update sender's last-seen time, adding it if there are no
      // higher-level protocols.
      table.observe(new ProtocolNode(sender, this), next == None)
      msg match {
        case ping @ PingMessage(_, _)             => handlePing(sender, ping)
        case lookup @ LookupMessage(_, _)         => handleLookup(sender, lookup)
        case disconnect @ DisconnectMessage(_, _) => handleDisconnect(sender, disconnect)
        case resp: ProtocolResponse               => handleResponse(sender, resp)
        case _                                    => next.foreach(_.dispatch(msg))
      }
    }

  def add(peer: PeerNode): Unit = table.observe(new ProtocolNode(peer, this), true)

  /**
    *
    */
  private def handleResponse(sender: PeerNode, msg: ProtocolResponse): Unit =
    for {
      ret <- msg.returnHeader
    } {
      pending.get(PendingKey(sender.key, ret.timestamp, ret.seq)) match {
        case Some(promise) => promise.success(Right(msg))
        case None => next.foreach(_.dispatch(msg))
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
      id <- lookup.lookupId
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
  override def roundTrip(
      msg: ProtocolMessage,
      remote: ProtocolNode,
      timeout: Duration = Duration(500, MILLISECONDS)): Either[CommError, ProtocolMessage] =
    msg.header match {
      case Some(header) => {
        val bytes = msg.toByteSeq
        val pend = PendingKey(remote.key, header.timestamp, header.seq)
        val promise = Promise[Either[CommError, ProtocolMessage]]
        pending.put(pend, promise)
        try {
          comm.send(bytes, remote)
          Await.result(promise.future, timeout)
        } catch {
          case ex: Exception => Left(ProtocolException(ex))
        } finally {
          pending.remove(pend)
          ()
        }
      }
      case None => Left(UnknownProtocolError("malformed message"))
    }

  receiver.start

  override def toString = s"#{Network $local ${local.endpoint.udpSocketAddress}}"
}
