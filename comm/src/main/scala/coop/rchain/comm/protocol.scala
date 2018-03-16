package coop.rchain.comm

import coop.rchain.kademlia
import coop.rchain.comm.protocol.routing._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import scala.concurrent.duration.{Duration, MILLISECONDS}
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm._, CommError._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import kamon._

// TODO: In message construction, the system clock is used for nonce
// generation. For reproducibility, this should be a passed-in value.

// TODO REMOVE inheritance hierarchy for composition
trait ProtocolDispatcher[A] {

  /**
    * Handle an incoming message. This function is intended to thread
    * levels of protocol together, such that inner protocols can
    * bubble unhandled messages up to outer levels.
    */
  def dispatch[F[_]: Monad: Capture: Log: Time: Metrics: Communication: Encryption: Kvs[
                 ?[_],
                 PeerNode,
                 Array[Byte]]: ApplicativeError_[?[_], CommError]](extra: A,
                                                                   msg: ProtocolMessage): F[Unit]
}

/**
  * Implements broadcasting and round-trip (request-response) messaging
  * for higher level protocols.
  */
trait ProtocolHandler {

  /**
    * The node that anchors this handler; `local` becomes the source
    * for outgoing communications.
    */
  def local: ProtocolNode

  /**
    * Send a message to a single, remote node, and wait up to the
    * specified duration for a response.
    */
  def roundTrip[F[_]: Capture: Monad](
      msg: ProtocolMessage,
      remote: ProtocolNode,
      timeout: Duration = Duration(500, MILLISECONDS)): F[Either[CommError, ProtocolMessage]]

  /**
    * Asynchronously broadcast a message to all known peers.
    */
  def broadcast(msg: ProtocolMessage): Seq[Either[CommError, Unit]]
}

object ProtocolNode {

  def apply(peer: PeerNode,
            local: ProtocolNode,
            roundTrip: (ProtocolMessage, ProtocolNode) => CommErr[ProtocolMessage]): ProtocolNode =
    new ProtocolNode(peer.id, peer.endpoint, Some(local), roundTrip)

  def apply(peer: PeerNode,
            roundTrip: (ProtocolMessage, ProtocolNode) => CommErr[ProtocolMessage]): ProtocolNode =
    new ProtocolNode(peer.id, peer.endpoint, None, roundTrip)
}

/**
  * A `PeerNode` that knows how to send and receive messages. The
  * `ping` method for Kademlia is here.
  */
class ProtocolNode private (id: NodeIdentifier,
                            endpoint: Endpoint,
                            maybeLocal: Option[ProtocolNode],
                            roundTrip: (ProtocolMessage, ProtocolNode) => CommErr[ProtocolMessage])
    extends PeerNode(id, endpoint)
    with kademlia.Peer {

  def local: ProtocolNode = maybeLocal.getOrElse(this)

  private var _seq = 0L
  def seq: Long = _seq synchronized {
    _seq += 1
    _seq
  }

  val pingSendCount   = Kamon.counter("protocol-ping-sends")
  val lookupSendCount = Kamon.counter("protocol-lookup-send")

  override def ping: Try[Duration] = {
    pingSendCount.increment()
    val req = PingMessage(ProtocolMessage.ping(local), System.currentTimeMillis)
    roundTrip(req, this) match {
      case Right(resp) =>
        req.header match {
          case Some(incoming) =>
            Success(Duration(resp.timestamp - incoming.timestamp, MILLISECONDS))
          case _ => Failure(new Exception("ping failed"))
        }
      case Left(ex) =>
        ex match {
          case ProtocolException(exc) => Failure(exc)
          case exc                    => Failure(new Exception(exc.toString))
        }
    }
  }

  def lookup(key: Seq[Byte]): Try[Seq[PeerNode]] = {
    lookupSendCount.increment()
    val req = LookupMessage(ProtocolMessage.lookup(local, key), System.currentTimeMillis)
    roundTrip(req, this) match {
      case Right(LookupResponseMessage(proto, _)) =>
        proto.message.lookupResponse match {
          case Some(resp) => Success(resp.nodes.map(ProtocolMessage.toPeerNode(_)))
          case _          => Success(Seq())
        }
      case Right(other) => Failure(new Exception("unexpected response"))
      case Left(ex) =>
        ex match {
          case ProtocolException(exc) => Failure(exc)
          case exc                    => Failure(new Exception(exc.toString))
        }
    }
  }
}

/**
  * `ProtocolMessage` insulates protocol handlers from protocol buffer
  * clutter.
  */
trait ProtocolMessage {
  val proto: Protocol
  val timestamp: Long

  def header: Option[Header] = proto.header

  def sender: Option[PeerNode] =
    for {
      h <- header
      s <- h.sender
    } yield
      new PeerNode(NodeIdentifier(s.id.toByteArray),
                   Endpoint(s.host.toStringUtf8, s.tcpPort, s.udpPort))

  def toByteSeq: Seq[Byte] =
    proto.toByteArray
}

/**
  * Supports return headers, which hold information about the message
  * being responded to.
  */
trait ProtocolResponse extends ProtocolMessage {
  def returnHeader: Option[ReturnHeader] = proto.returnHeader
}

/**
  * A ping is a simple are-you-there? message.
  */
final case class PingMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage {
  def response(src: ProtocolNode): Option[ProtocolMessage] =
    for {
      h <- header
    } yield PongMessage(ProtocolMessage.pong(src, h), System.currentTimeMillis)
}

/**
  * A pong is the response to a ping.
  */
final case class PongMessage(proto: Protocol, timestamp: Long) extends ProtocolResponse

/**
  * A lookup message asks for a list of peers from the local Kademlia
  * table that are closest to a given key.
  */
final case class LookupMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage {
  def lookupId: Option[Seq[Byte]] = proto.message.lookup.map(_.id.toByteArray)

  def response(src: ProtocolNode, nodes: Seq[PeerNode]): Option[ProtocolMessage] =
    header.map { h =>
      LookupResponseMessage(ProtocolMessage.lookupResponse(src, h, nodes), System.currentTimeMillis)
    }
}

/**
  * A disconnect causes the receiver to forget about this peer.
  */
final case class DisconnectMessage(proto: Protocol, timestamp: Long) extends ProtocolMessage

/**
  * The response to a lookup message. It holds the list of peers
  * closest to the queried key.
  */
final case class LookupResponseMessage(proto: Protocol, timestamp: Long) extends ProtocolResponse

final case class UpstreamMessage(proto: Protocol, timestamp: Long)  extends ProtocolMessage
final case class UpstreamResponse(proto: Protocol, timestamp: Long) extends ProtocolResponse

/**
  * Utility functions for working with protocol buffers.
  */
object ProtocolMessage {

  implicit def toProtocolBytes(x: String) =
    com.google.protobuf.ByteString.copyFromUtf8(x)
  implicit def toProtocolBytes(x: Array[Byte]) =
    com.google.protobuf.ByteString.copyFrom(x)
  implicit def toProtocolBytes(x: Seq[Byte]) =
    com.google.protobuf.ByteString.copyFrom(x.toArray)

  def header(src: ProtocolNode): Header =
    Header()
      .withSender(node(src))
      .withTimestamp(System.currentTimeMillis)
      .withSeq(src.seq)

  def node(n: PeerNode): Node =
    Node()
      .withId(n.key)
      .withHost(n.endpoint.host)
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)

  def toPeerNode(n: Node): PeerNode =
    new PeerNode(NodeIdentifier(n.id.toByteArray),
                 Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def returnHeader(h: Header): ReturnHeader =
    ReturnHeader()
      .withTimestamp(h.timestamp)
      .withSeq(h.seq)

  def ping(src: ProtocolNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withPing(Ping())

  def pong(src: ProtocolNode, h: Header): Protocol =
    Protocol()
      .withHeader(header(src))
      .withReturnHeader(returnHeader(h))
      .withPong(Pong())

  def lookup(src: ProtocolNode, id: Seq[Byte]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withLookup(Lookup()
        .withId(id.toArray))

  def lookupResponse(src: ProtocolNode, h: Header, nodes: Seq[PeerNode]): Protocol =
    Protocol()
      .withHeader(header(src))
      .withReturnHeader(returnHeader(h))
      .withLookupResponse(LookupResponse()
        .withNodes(nodes.map(node(_))))

  def disconnect(src: ProtocolNode): Protocol =
    Protocol()
      .withHeader(header(src))
      .withDisconnect(Disconnect())

  def upstreamMessage(src: ProtocolNode, upstream: AnyProto): Protocol =
    Protocol()
      .withHeader(header(src))
      .withUpstream(upstream)

  def upstreamResponse(src: ProtocolNode, h: Header, upstream: AnyProto): Protocol =
    Protocol()
      .withHeader(header(src))
      .withReturnHeader(returnHeader(h))
      .withUpstream(upstream)

  def parse(bytes: Seq[Byte]): Either[CommError, ProtocolMessage] =
    try {
      Protocol.parseFrom(bytes.toArray) match {
        case msg: Protocol =>
          msg.message match {
            case Protocol.Message.Ping(_)   => Right(PingMessage(msg, System.currentTimeMillis))
            case Protocol.Message.Pong(_)   => Right(PongMessage(msg, System.currentTimeMillis))
            case Protocol.Message.Lookup(_) => Right(LookupMessage(msg, System.currentTimeMillis))
            case Protocol.Message.LookupResponse(_) =>
              Right(LookupResponseMessage(msg, System.currentTimeMillis))
            case Protocol.Message.Disconnect(_) =>
              Right(DisconnectMessage(msg, System.currentTimeMillis))
            case Protocol.Message.Upstream(_) =>
              msg.returnHeader match {
                case Some(_) => Right(UpstreamResponse(msg, System.currentTimeMillis))
                case None    => Right(UpstreamMessage(msg, System.currentTimeMillis))
              }

            case _ => Left(UnknownProtocolError("unable to unmarshal protocol buffer"))
          }
      }
    } catch {
      case NonFatal(ex: Exception) => Left(ProtocolException(ex))
    }
}
