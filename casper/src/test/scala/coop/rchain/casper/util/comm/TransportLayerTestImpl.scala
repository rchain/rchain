package coop.rchain.casper.util.comm

import cats.{Id, Monad}
import cats.implicits._

import coop.rchain.comm.protocol.routing._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.{peerNodeNotFound, CommErr}
import coop.rchain.comm.{PeerNode, ProtocolMessage}
import coop.rchain.p2p.effects._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.metrics.Metrics

import scala.concurrent.duration.Duration
import scala.collection.mutable
import coop.rchain.comm.transport._

class TransportLayerTestImpl[F[_]: Monad: Capture](
    identity: PeerNode,
    val msgQueues: collection.Map[PeerNode, mutable.Queue[Protocol]])
    extends TransportLayer[F] {

  def roundTrip(msg: Protocol, remote: PeerNode, timeout: Duration): F[CommErr[Protocol]] = ???

  def local: F[PeerNode] = identity.pure[F]

  def send(msg: Protocol, peer: PeerNode): F[CommErr[Unit]] = Capture[F].capture {
    val maybeQ = msgQueues.get(peer)

    maybeQ.fold[CommErr[Unit]](Left(peerNodeNotFound(peer)))(q => Right(q.enqueue(msg)))
  }

  def broadcast(msg: Protocol, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] = ???

  def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit] =
    TransportLayerTestImpl.handleQueue(dispatch, msgQueues(identity))
}

object TransportLayerTestImpl {
  def handleQueue[F[_]: Monad: Capture](dispatch: Protocol => F[CommunicationResponse],
                                        q: mutable.Queue[Protocol]): F[Unit] =
    if (q.nonEmpty) for {
      proto <- Capture[F].capture { q.dequeue() }
      _     <- dispatch(proto)
      _     <- handleQueue(dispatch, q)
    } yield ()
    else ().pure[F]
}
