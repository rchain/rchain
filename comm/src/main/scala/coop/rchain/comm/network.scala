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

  private def dispatch[F[_]: Monad: Capture: Log: Time: Metrics](
      sock: SocketAddress,
      msg: ProtocolMessage): F[Option[ProtocolMessage]] = {

    val dispatchForSender: Option[F[Option[ProtocolMessage]]] = msg.sender.map { sndr =>
      val sender = pumpSender(sock, sndr)
      msg match {
        case resp: ProtocolResponse => handleResponse[F](sock, sender, resp)
        case _                      => msg.some.pure[F]
      }
    }

    dispatchForSender.getOrElse(Log[F].error("Tried to dispatch message without a sender").as(None))
  }

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

  def pumpSender(sock: SocketAddress, sndr: PeerNode): PeerNode = sock match {
    case (s: java.net.InetSocketAddress) =>
      sndr.withUdpSocket(s)
    case _ => sndr
  }

  override def toString = s"#{Network $local ${local.endpoint.udpSocketAddress}}"
}
