package coop.rchain.casper.util.comm

import cats.{Id, Monad}
import cats.implicits._

import coop.rchain.comm.protocol.routing._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.{peerNodeNotFound, CommErr}
import coop.rchain.comm.{CommError, PeerNode}
import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable

import coop.rchain.comm.transport._

class TransportLayerTestImpl[F[_]: Monad: Capture](
    identity: PeerNode,
    val msgQueues: collection.Map[PeerNode, mutable.Queue[Protocol]])
    extends TransportLayer[F] {

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]] =
    Capture[F].capture {
      val maybeQ = msgQueues.get(peer)
      maybeQ.fold[CommErr[Protocol]](Left(peerNodeNotFound(peer)))(q =>
        Right(q.enqueue(msg)).map(_ => Protocol()))
    }

  def local: F[PeerNode] = identity.pure[F]

  def send(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[Unit] =
    roundTrip(peer, msg, timeout).void

  def broadcast(peers: Seq[PeerNode], msg: Protocol, timeout: FiniteDuration): F[Unit] =
    peers.toList.traverse(send(_, msg, timeout)).void

  def safeBroadcast(peers: Seq[PeerNode],
                    msg: Protocol,
                    timeout: FiniteDuration): F[Map[PeerNode, Option[CommError]]] =
    peers.toList
      .traverse(p => roundTrip(p, msg, timeout).map(p -> _.swap.toOption))
      .map(_.toMap)

  def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit] =
    TransportLayerTestImpl.handleQueue(dispatch, msgQueues(identity))

  def disconnect(peer: PeerNode): F[Unit] = ???
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
