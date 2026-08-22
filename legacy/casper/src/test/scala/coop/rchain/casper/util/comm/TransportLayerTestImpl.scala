package coop.rchain.casper.util.comm

import cats.Monad
import cats.effect._
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.util.comm.TestNetwork.TestNetwork
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper.protocol
import coop.rchain.comm.transport._
import coop.rchain.comm.{CommError, PeerNode}

import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration

object TestNetwork {
  type NodeMessageQueues = Map[PeerNode, Queue[Protocol]]
  type TestNetwork[F[_]] = Ref[F, NodeMessageQueues]

  def apply[F[_]](implicit ev: TestNetwork[F]): TestNetwork[F] = ev

  def addPeer[F[_]: TestNetwork](peer: PeerNode): F[Unit] =
    TestNetwork[F].update(t => t + (peer -> Queue.empty))

  def peerQueue[F[_]: TestNetwork: Monad](peer: PeerNode): F[Queue[Protocol]] =
    TestNetwork[F].get.map(_(peer))

  def send[F[_]: TestNetwork](peer: PeerNode, msg: Protocol): F[Unit] =
    TestNetwork[F].update(t => { t + (peer -> t(peer).enqueue(msg)) })

  def clear[F[_]: TestNetwork](peer: PeerNode): F[Unit] =
    TestNetwork[F].update(t => { t + (peer -> Queue.empty) })

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
              TestNetwork[F].set(topology + (peer -> nq)) >>
                dispatch(p) >>
                handleQueue(dispatch, peer)
          }
    } yield ()

  def empty[F[_]: Sync]: TestNetwork[F] = Ref.unsafe[F, NodeMessageQueues](Map())
}

class TransportLayerTestImpl[F[_]: Monad: TestNetwork]() extends TransportLayer[F] {
  val testNetworkF = TestNetwork[F]

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]] = ???

  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]] =
    TestNetwork.send(peer, msg).map(_.asRight[CommError])

  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]] =
    peers.toList.traverse(send(_, msg)).map(_.toSeq)

  def stream(peer: PeerNode, blob: Blob): F[Unit] =
    stream(Seq(peer), blob)

  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit] =
    broadcast(peers, protocol(blob.sender, "test").withPacket(blob.packet)).void

  def clear(peer: PeerNode): F[Unit] =
    TestNetwork.clear(peer)
}

class TransportLayerServerTestImpl[F[_]: Sync: TestNetwork](identity: PeerNode)
    extends TransportLayerServer[F] {
  def resource(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): Resource[F, Unit] =
    Resource.eval(TestNetwork.handleQueue(dispatch, identity))
}
