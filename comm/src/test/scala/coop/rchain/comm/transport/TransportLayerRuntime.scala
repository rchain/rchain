package coop.rchain.comm.transport

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Random

import cats._
import cats.effect.Timer
import cats.effect.concurrent.MVar
import cats.implicits._

import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.rp.ProtocolHelper

abstract class TransportLayerRuntime[F[_]: Monad: Timer, E <: Environment] {

  def createEnvironment(port: Int): F[E]

  def createTransportLayer(env: E): F[TransportLayer[F]]

  def createDispatcherCallback: F[DispatcherCallback[F]]

  def extract[A](fa: F[A]): A

  private val nextPort = new AtomicInteger((Random.nextInt(100) + 400 + 1) * 100)

  def twoNodesEnvironment[A](block: (E, E) => F[A]): F[A] =
    for {
      e1 <- createEnvironment(nextPort.incrementAndGet())
      e2 <- createEnvironment(nextPort.incrementAndGet())
      r  <- block(e1, e2)
    } yield r

  def threeNodesEnvironment[A](block: (E, E, E) => F[A]): F[A] =
    for {
      e1 <- createEnvironment(nextPort.incrementAndGet())
      e2 <- createEnvironment(nextPort.incrementAndGet())
      e3 <- createEnvironment(nextPort.incrementAndGet())
      r  <- block(e1, e2, e3)
    } yield r

  trait Runtime[A] {
    protected def protocolDispatcher: Dispatcher[F, Protocol, CommunicationResponse]
    protected def streamDispatcher: Dispatcher[F, Blob, Unit]
    def run(blockUntilDispatched: Boolean): Result
    trait Result {
      def localNode: PeerNode
      def apply(): A
    }
  }

  abstract class TwoNodesRuntime[A](
      val protocolDispatcher: Dispatcher[F, Protocol, CommunicationResponse] =
        Dispatcher.withoutMessageDispatcher[F],
      val streamDispatcher: Dispatcher[F, Blob, Unit] = Dispatcher.devNullPacketDispatcher[F]
  ) extends Runtime[A] {
    def execute(transportLayer: TransportLayer[F], local: PeerNode, remote: PeerNode): F[A]

    def run(blockUntilDispatched: Boolean = true): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            localTl  <- createTransportLayer(e1)
            remoteTl <- createTransportLayer(e2)
            local    = e1.peer
            remote   = e2.peer
            cbl      <- createDispatcherCallback
            cb       <- createDispatcherCallback
            _ <- localTl.receive(
                  Dispatcher.withoutMessageDispatcher[F].dispatch(local, cbl),
                  Dispatcher.devNullPacketDispatcher[F].dispatch(local, cbl)
                )
            _ <- remoteTl.receive(
                  protocolDispatcher.dispatch(remote, cb),
                  streamDispatcher.dispatch(remote, cb)
                )
            r <- execute(localTl, local, remote)
            _ <- if (blockUntilDispatched) cb.waitUntilDispatched()
                else implicitly[Timer[F]].sleep(1.second)
            _ <- remoteTl.shutdown(ProtocolHelper.disconnect(remote))
            _ <- localTl.shutdown(ProtocolHelper.disconnect(local))
          } yield
            new TwoNodesResult {
              def localNode: PeerNode        = local
              def remoteNode: PeerNode       = remote
              def remoteNodes: Seq[PeerNode] = Seq(remote)
              def apply(): A                 = r
            }
        }
      )

    trait TwoNodesResult extends Result {
      def remoteNode: PeerNode
    }
  }

  abstract class TwoNodesRemoteDeadRuntime[A](
      val protocolDispatcher: Dispatcher[F, Protocol, CommunicationResponse] =
        Dispatcher.withoutMessageDispatcher[F],
      val streamDispatcher: Dispatcher[F, Blob, Unit] = Dispatcher.devNullPacketDispatcher[F]
  ) extends Runtime[A] {
    def execute(transportLayer: TransportLayer[F], local: PeerNode, remote: PeerNode): F[A]

    def run(blockUntilDispatched: Boolean = false): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            localTl <- createTransportLayer(e1)
            local   = e1.peer
            remote  = e2.peer
            cbl     <- createDispatcherCallback
            _ <- localTl.receive(
                  Dispatcher.withoutMessageDispatcher[F].dispatch(local, cbl),
                  Dispatcher.devNullPacketDispatcher[F].dispatch(local, cbl)
                )
            r <- execute(localTl, local, remote)
            _ <- localTl.shutdown(ProtocolHelper.disconnect(local))
          } yield
            new TwoNodesResult {
              def localNode: PeerNode  = local
              def remoteNode: PeerNode = remote
              def apply(): A           = r
            }
        }
      )

    trait TwoNodesResult extends Result {
      def remoteNode: PeerNode
    }
  }

  abstract class ThreeNodesRuntime[A](
      val protocolDispatcher: Dispatcher[F, Protocol, CommunicationResponse] =
        Dispatcher.withoutMessageDispatcher[F],
      val streamDispatcher: Dispatcher[F, Blob, Unit] = Dispatcher.devNullPacketDispatcher[F]
  ) extends Runtime[A] {
    def execute(
        transportLayer: TransportLayer[F],
        local: PeerNode,
        remote1: PeerNode,
        remote2: PeerNode
    ): F[A]

    def run(blockUntilDispatched: Boolean = true): ThreeNodesResult =
      extract(
        threeNodesEnvironment { (e1, e2, e3) =>
          for {
            localTl   <- createTransportLayer(e1)
            remoteTl1 <- createTransportLayer(e2)
            remoteTl2 <- createTransportLayer(e3)
            local     = e1.peer
            remote1   = e2.peer
            remote2   = e3.peer
            cbl       <- createDispatcherCallback
            cb1       <- createDispatcherCallback
            cb2       <- createDispatcherCallback
            _ <- localTl.receive(
                  Dispatcher.withoutMessageDispatcher[F].dispatch(local, cbl),
                  Dispatcher.devNullPacketDispatcher[F].dispatch(local, cbl)
                )
            _ <- remoteTl1.receive(
                  protocolDispatcher.dispatch(remote1, cb1),
                  streamDispatcher.dispatch(remote1, cb1)
                )
            _ <- remoteTl2.receive(
                  protocolDispatcher.dispatch(remote2, cb2),
                  streamDispatcher.dispatch(remote2, cb2)
                )
            r <- execute(localTl, local, remote1, remote2)
            _ <- if (blockUntilDispatched) cb1.waitUntilDispatched()
                else implicitly[Timer[F]].sleep(1.second)
            _ <- if (blockUntilDispatched) cb2.waitUntilDispatched()
                else implicitly[Timer[F]].sleep(1.second)
            _ <- remoteTl1.shutdown(ProtocolHelper.disconnect(remote1))
            _ <- remoteTl2.shutdown(ProtocolHelper.disconnect(remote2))
            _ <- localTl.shutdown(ProtocolHelper.disconnect(local))
          } yield
            new ThreeNodesResult {
              def localNode: PeerNode   = local
              def remoteNode1: PeerNode = remote1
              def remoteNode2: PeerNode = remote2
              def apply(): A            = r
            }
        }
      )

    trait ThreeNodesResult extends Result {
      def remoteNode1: PeerNode
      def remoteNode2: PeerNode
    }
  }

  def roundTripWithHeartbeat(
      transport: TransportLayer[F],
      local: PeerNode,
      remote: PeerNode,
      timeout: FiniteDuration = 10.second
  ): F[CommErr[Protocol]] = {
    val msg = ProtocolHelper.heartbeat(local)
    transport.roundTrip(remote, msg, timeout)
  }

  def sendHeartbeat(
      transport: TransportLayer[F],
      local: PeerNode,
      remote: PeerNode
  ): F[CommErr[Unit]] = {
    val msg = ProtocolHelper.heartbeat(local)
    transport.send(remote, msg)
  }

  def broadcastHeartbeat(
      transport: TransportLayer[F],
      local: PeerNode,
      remotes: PeerNode*
  ): F[Seq[CommErr[Unit]]] = {
    val msg = ProtocolHelper.heartbeat(local)
    transport.broadcast(remotes, msg)
  }

}

trait Environment {
  def peer: PeerNode
  def host: String
  def port: Int
}

final class DispatcherCallback[F[_]: Functor](state: MVar[F, Unit]) {
  def notifyThatDispatched(): F[Unit] = state.tryPut(()).void
  def waitUntilDispatched(): F[Unit]  = state.take
}

final class Dispatcher[F[_]: Monad: Timer, R, S](
    response: PeerNode => S,
    delay: Option[FiniteDuration] = None,
    ignore: R => Boolean = kp(false)
) {
  def dispatch(peer: PeerNode, callback: DispatcherCallback[F]): R => F[S] =
    p =>
      for {
        _ <- delay.fold(().pure[F])(implicitly[Timer[F]].sleep)
        _ = if (!ignore(p)) receivedMessages.synchronized(receivedMessages += ((peer, p)))
        r = response(peer)
        _ <- callback.notifyThatDispatched()
      } yield r

  def received: Seq[(PeerNode, R)] = receivedMessages
  private val receivedMessages     = mutable.MutableList.empty[(PeerNode, R)]
}

object Dispatcher {
  def heartbeatResponseDispatcher[F[_]: Monad: Timer]
    : Dispatcher[F, Protocol, CommunicationResponse] =
    new Dispatcher[F, Protocol, CommunicationResponse](
      peer => CommunicationResponse.handledWithMessage(ProtocolHelper.heartbeatResponse(peer)),
      ignore = _.message.isDisconnect
    )

  def heartbeatResponseDispatcherWithDelay[F[_]: Monad: Timer](
      delay: FiniteDuration
  ): Dispatcher[F, Protocol, CommunicationResponse] =
    new Dispatcher[F, Protocol, CommunicationResponse](
      peer => CommunicationResponse.handledWithMessage(ProtocolHelper.heartbeatResponse(peer)),
      delay = Some(delay),
      ignore = _.message.isDisconnect
    )

  def withoutMessageDispatcher[F[_]: Monad: Timer]: Dispatcher[F, Protocol, CommunicationResponse] =
    new Dispatcher[F, Protocol, CommunicationResponse](
      _ => CommunicationResponse.handledWithoutMessage,
      ignore = _.message.isDisconnect
    )

  def internalCommunicationErrorDispatcher[F[_]: Monad: Timer]
    : Dispatcher[F, Protocol, CommunicationResponse] =
    new Dispatcher[F, Protocol, CommunicationResponse](
      _ => CommunicationResponse.notHandled(InternalCommunicationError("Test")),
      ignore = _.message.isDisconnect
    )

  def devNullPacketDispatcher[F[_]: Monad: Timer]: Dispatcher[F, Blob, Unit] =
    new Dispatcher[F, Blob, Unit](response = kp(()))
}
