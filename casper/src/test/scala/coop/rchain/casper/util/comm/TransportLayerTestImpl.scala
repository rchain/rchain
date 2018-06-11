package coop.rchain.casper.util.comm

import cats.{Id, Monad}
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.{peerNodeNotFound, CommErr}
import coop.rchain.comm.{PeerNode, ProtocolMessage}
import coop.rchain.p2p.effects._
import coop.rchain.p2p.FrameMessage
import coop.rchain.p2p.Network.{ErrorHandler, KeysStore}
import coop.rchain.metrics.Metrics

import scala.concurrent.duration.Duration
import scala.collection.mutable

class TransportLayerTestImpl[F[_]: Monad: Capture](
    identity: PeerNode,
    val msgQueues: collection.Map[PeerNode, mutable.Queue[ProtocolMessage]])
    extends TransportLayer[F] {

  def roundTrip(msg: ProtocolMessage,
                remote: PeerNode,
                timeout: Duration): F[CommErr[ProtocolMessage]] = ???

  def local: F[PeerNode] = identity.pure[F]

  def send(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] = Capture[F].capture {
    val maybeQ = msgQueues.get(peer)

    maybeQ.fold[CommErr[Unit]](Left(peerNodeNotFound(peer)))(q =>
      ProtocolMessage.toProtocolMessage(msg.proto).map(q.enqueue(_)))
  }

  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] = ???

  def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit] =
    TransportLayerTestImpl.handleQueue(dispatch, msgQueues(identity))
}

object TransportLayerTestImpl {
  def handleQueue[F[_]: Monad: Capture](dispatch: (ProtocolMessage) => F[CommunicationResponse],
                                        q: mutable.Queue[ProtocolMessage]): F[Unit] =
    if (q.nonEmpty) for {
      msg <- Capture[F].capture { q.dequeue() }
      _   <- dispatch(msg)
      _   <- handleQueue(dispatch, q)
    } yield ()
    else ().pure[F]
}
