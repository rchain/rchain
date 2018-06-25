package coop.rchain.casper.util.comm

import cats.{Id, Monad}
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.{peerNodeNotFound, CommErr}
import coop.rchain.comm.{PeerNode, ProtocolMessage}

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable
import coop.rchain.comm.transport._

class TransportLayerTestImpl[F[_]: Monad: Capture](
    identity: PeerNode,
    val msgQueues: collection.Map[PeerNode, mutable.Queue[ProtocolMessage]])
    extends TransportLayer[F] {

  def roundTrip(peer: PeerNode,
                msg: ProtocolMessage,
                timeout: FiniteDuration): F[CommErr[ProtocolMessage]] = ???

  def local: F[PeerNode] = identity.pure[F]

  def send(peer: PeerNode, msg: ProtocolMessage): F[CommErr[Unit]] = Capture[F].capture {
    val maybeQ = msgQueues.get(peer)

    maybeQ.fold[CommErr[Unit]](Left(peerNodeNotFound(peer)))(q =>
      ProtocolMessage.toProtocolMessage(msg.proto).map(q.enqueue(_)))
  }

  def broadcast(peers: Seq[PeerNode], msg: ProtocolMessage): F[Seq[CommErr[Unit]]] = ???

  def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit] =
    TransportLayerTestImpl.handleQueue(dispatch, msgQueues(identity))

  def disconnect(peer: PeerNode): F[Unit] = ???
}

object TransportLayerTestImpl {
  def handleQueue[F[_]: Monad: Capture](dispatch: ProtocolMessage => F[CommunicationResponse],
                                        q: mutable.Queue[ProtocolMessage]): F[Unit] =
    if (q.nonEmpty) for {
      msg <- Capture[F].capture { q.dequeue() }
      _   <- dispatch(msg)
      _   <- handleQueue(dispatch, q)
    } yield ()
    else ().pure[F]
}
