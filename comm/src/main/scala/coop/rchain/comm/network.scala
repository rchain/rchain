package coop.rchain.comm

import java.net.SocketTimeoutException
import scala.collection.concurrent
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.kademlia.PeerTable

/**
  * Implements the lower levels of the network protocol.
  */
case class UnicastNetwork(id: NodeIdentifier, endpoint: Endpoint) extends ProtocolHandler {

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
          case Left(err: CommError) => err match {
            case DatagramException(ex: SocketTimeoutException) => ()
            // These next ones may ding a node's reputation; just
            // printing for now.
            case err @ DatagramSizeError(sz) => println(s"bad size $sz")
            case err @ DatagramFramingError(ex) => ex.printStackTrace
            case err @ DatagramException(ex) => ex.printStackTrace

            case _ => ()
          }
        }
      }
  }

  private def dispatch(msg: ProtocolMessage): Unit =
    for {
      sender <- msg.sender
    } {
      table.observe(new ProtocolNode(sender, this))
      msg match {
        case ping @ PingMessage(_, _)     => handlePing(sender, ping)
        case pong @ PongMessage(_, _)     => handlePong(sender, pong)
        case lookup @ LookupMessage(_, _) => handleLookup(sender, lookup)
        case lookupResponse @ LookupResponseMessage(_, _) =>
          handleLookupResponse(sender, lookupResponse)
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
    * Validate incoming PONG and complete its pending promise.
    */
  private def handlePong(sender: PeerNode, pong: PongMessage): Unit =
    for {
      ret <- pong.returnHeader
      promise <- pending.get(PendingKey(sender.key, ret.timestamp, ret.seq))
    } {
      try {
        promise.success(Right(pong))
      } catch {
        case ex: java.lang.IllegalStateException => () // Future already completed
      }
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
    * Validate and handle incoming LOOKUP_RESPONSE message.
    */
  private def handleLookupResponse(sender: PeerNode, response: LookupResponseMessage): Unit =
    for {
      ret <- response.returnHeader
      promise <- pending.get(PendingKey(sender.key, ret.timestamp, ret.seq))
    } {
      try {
        promise.success(Right(response))
      } catch {
        case ex: java.lang.IllegalStateException => () // Future already completed
      }
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
  override def roundTrip(msg: ProtocolMessage,
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
