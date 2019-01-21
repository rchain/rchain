package coop.rchain.casper.util.comm

import cats.Monad
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl.MonadState
import coop.rchain.comm.protocol.routing._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.ProtocolHelper.protocol

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable
import coop.rchain.comm.transport._
import coop.rchain.shared.AtomicMonadState
import monix.execution.atomic.AtomicAny

import scala.collection.immutable.Queue

object TestNetwork {
  type NodeMessageQueues = Map[PeerNode, Queue[Protocol]]
  type TestNetwork[F[_]] = MonadState[F, NodeMessageQueues]

  def apply[F[_]](implicit ev: TestNetwork[F]): TestNetwork[F] = ev

  def addPeer[F[_]: TestNetwork](peer: PeerNode): F[Unit] =
    TestNetwork[F].modify(t => t + (peer -> Queue.empty))

  def peerQueue[F[_]: TestNetwork: Monad](peer: PeerNode): F[Queue[Protocol]] =
    TestNetwork[F].inspect(_(peer))

  def send[F[_]: TestNetwork](peer: PeerNode, msg: Protocol): F[Unit] =
    TestNetwork[F].modify(t => {
      t + (peer -> t(peer).enqueue(msg))
    })

  def clear[F[_]: TestNetwork](peer: PeerNode): F[Unit] =
    TestNetwork[F].modify(t => {
      t + (peer -> Queue.empty)
    })

  def handleQueue[F[_]: TestNetwork: Monad](
      dispatch: Protocol => F[CommunicationResponse],
      peer: PeerNode
  ): F[Unit] =
    for {
      topology     <- TestNetwork[F].get
      q            = topology(peer)
      maybeMessage = q.dequeueOption
      _ <- maybeMessage.fold(().pure[F]) {
            case (p, nq) =>
              TestNetwork[F].set(topology + (peer -> nq)) *>
                dispatch(p) *>
                handleQueue(dispatch, peer)
          }
    } yield ()

  def empty[F[_]](implicit captureF: Capture[F], monadF: Monad[F]): TestNetwork[F] =
    new AtomicMonadState[F, NodeMessageQueues](AtomicAny(Map.empty))
}

class TransportLayerTestImpl[F[_]: Monad](
    identity: PeerNode,
    val msgQueues: Map[PeerNode, Ref[F, mutable.Queue[Protocol]]]
) extends TransportLayer[F] {

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]] = ???

  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]] =
    msgQueues.get(peer) match {
      case Some(qRef) =>
        qRef
          .update { q =>
            q.enqueue(msg); q
          }
          .map(Right(_))
      case None => ().pure[F].map(Right(_))
    }

  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]] =
    peers.toList.traverse(send(_, msg)).map(_.toSeq)

  def receive(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): F[Unit] =
    TransportLayerTestImpl.handleQueue(dispatch, msgQueues(identity))

  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit] =
    broadcast(peers, protocol(blob.sender).withPacket(blob.packet)).void

  def disconnect(peer: PeerNode): F[Unit] = ???

  def shutdown(msg: Protocol): F[Unit] = ???

  def clear(peer: PeerNode): F[Unit] =
    msgQueues.get(peer) match {
      case Some(qRef) =>
        qRef.update { q =>
          q.clear(); q
        }
      case None => ().pure[F]
    }
}

object TransportLayerTestImpl {
  def handleQueue[F[_]: Monad](
      dispatch: Protocol => F[CommunicationResponse],
      qRef: Ref[F, mutable.Queue[Protocol]]
  ): F[Unit] =
    for {
      maybeProto <- qRef.modify { q =>
                     if (q.nonEmpty) {
                       val proto = q.dequeue()
                       (q, proto.some)
                     } else (q, None)
                   }
      _ <- maybeProto match {
            case Some(proto) => dispatch(proto) *> handleQueue(dispatch, qRef)
            case None        => ().pure[F]
          }
    } yield ()
}
