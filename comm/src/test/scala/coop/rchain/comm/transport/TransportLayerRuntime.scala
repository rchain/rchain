package coop.rchain.comm.transport

import cats._
import cats.effect.{Async, Sync, Temporal}
import cats.syntax.all._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.ProtocolHelper

import java.net.ServerSocket
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Try, Using}
import cats.effect.std.PQueue

abstract class TransportLayerRuntime[F[_]: Async, E <: Environment] {

  val networkId = "test"

  def createEnvironment(port: Int): F[E]

  def createTransportLayer(env: E): F[TransportLayer[F]]

  def createTransportLayerServer(env: E): F[TransportLayerServer[F]]

  def createDispatcherCallback: F[DispatcherCallback[F]]

  def extract[A](fa: F[A]): A

  private def getFreePort: Try[Int] =
    Using(new ServerSocket(0)) { s =>
      s.setReuseAddress(true)
      s.getLocalPort
    }

  private def getFreePortF: F[Int] = Sync[F].fromTry(getFreePort)

  def twoNodesEnvironment[A](block: (E, E) => F[A]): F[A] =
    for {
      e1 <- getFreePortF >>= createEnvironment
      e2 <- getFreePortF >>= createEnvironment
      r  <- block(e1, e2)
    } yield r

  def threeNodesEnvironment[A](block: (E, E, E) => F[A]): F[A] =
    for {
      e1 <- getFreePortF >>= createEnvironment
      e2 <- getFreePortF >>= createEnvironment
      e3 <- getFreePortF >>= createEnvironment
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
    def execute(
        transportLayer: TransportLayer[F],
        local: PeerNode,
        remote: PeerNode
    ): F[A]

    def run(blockUntilDispatched: Boolean = true): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            tl        <- createTransportLayer(e1)
            localTl   = tl
            remoteTls <- createTransportLayerServer(e2)
            local     = e1.peer
            remote    = e2.peer
            cb        <- createDispatcherCallback
            server = remoteTls.resource(
              protocolDispatcher.dispatch(remote, cb),
              streamDispatcher.dispatch(remote, cb)
            )
            r <- server.use { _ =>
                  for {
                    r <- execute(localTl, local, remote)
                    _ <- if (blockUntilDispatched) cb.waitUntilDispatched()
                        else implicitly[Temporal[F]].sleep(1.second)
                  } yield r
                }
          } yield new TwoNodesResult {
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
    def execute(
        transportLayer: TransportLayer[F],
        local: PeerNode,
        remote: PeerNode
    ): F[A]

    def run(blockUntilDispatched: Boolean = false): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            tl      <- createTransportLayer(e1)
            localTl = tl
            local   = e1.peer
            remote  = e2.peer
            r       <- execute(localTl, local, remote)
          } yield new TwoNodesResult {
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
            tl         <- createTransportLayer(e1)
            localTl    = tl
            remoteTls1 <- createTransportLayerServer(e2)
            remoteTls2 <- createTransportLayerServer(e3)
            local      = e1.peer
            remote1    = e2.peer
            remote2    = e3.peer
            cbl        <- createDispatcherCallback
            cb1        <- createDispatcherCallback
            cb2        <- createDispatcherCallback
            server1 = remoteTls1.resource(
              protocolDispatcher.dispatch(remote1, cb1),
              streamDispatcher.dispatch(remote1, cb1)
            )
            server2 = remoteTls2.resource(
              protocolDispatcher.dispatch(remote2, cb2),
              streamDispatcher.dispatch(remote2, cb2)
            )
            r <- (server1 *> server2).use { _ =>
                  for {
                    r <- execute(localTl, local, remote1, remote2)
                    _ <- if (blockUntilDispatched) cb1.waitUntilDispatched()
                        else implicitly[Temporal[F]].sleep(1.second)
                    _ <- if (blockUntilDispatched) cb2.waitUntilDispatched()
                        else implicitly[Temporal[F]].sleep(1.second)
                  } yield r
                }
          } yield new ThreeNodesResult {
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

  def sendHeartbeat(
      transport: TransportLayer[F],
      local: PeerNode,
      remote: PeerNode
  ): F[CommErr[Unit]] = {
    val msg = ProtocolHelper.heartbeat(local, networkId)
    transport.send(remote, msg).map { x =>
      println(x)
      x
    }
  }

  def broadcastHeartbeat(
      transport: TransportLayer[F],
      local: PeerNode,
      remotes: PeerNode*
  ): F[Seq[CommErr[Unit]]] = {
    val msg = ProtocolHelper.heartbeat(local, networkId)
    transport.broadcast(remotes, msg)
  }

}

trait Environment {
  def peer: PeerNode
  def host: String
  def port: Int
}

final class DispatcherCallback[F[_]: Functor](state: PQueue[F, Unit]) {
  def notifyThatDispatched(): F[Unit] = state.tryOffer(()).void
  def waitUntilDispatched(): F[Unit]  = state.take
}

final class Dispatcher[F[_]: Monad: Temporal, R, S](
    response: PeerNode => S,
    delay: Option[FiniteDuration] = None,
    ignore: R => Boolean = kp(false)
) {
  def dispatch(peer: PeerNode, callback: DispatcherCallback[F]): R => F[S] =
    p =>
      for {
        _ <- delay.fold(().pure[F])(implicitly[Temporal[F]].sleep)
        _ = if (!ignore(p)) receivedMessages.synchronized(receivedMessages += ((peer, p)))
        r = response(peer)
        _ <- callback.notifyThatDispatched()
      } yield r

  def received: Seq[(PeerNode, R)] = receivedMessages
  private val receivedMessages     = mutable.MutableList.empty[(PeerNode, R)]
}

object Dispatcher {

  def withoutMessageDispatcher[F[_]: Monad: Temporal]
      : Dispatcher[F, Protocol, CommunicationResponse] =
    new Dispatcher[F, Protocol, CommunicationResponse](
      _ => CommunicationResponse.handledWithoutMessage,
      ignore = _.message.isDisconnect
    )

  def internalCommunicationErrorDispatcher[F[_]: Monad: Temporal]
      : Dispatcher[F, Protocol, CommunicationResponse] =
    new Dispatcher[F, Protocol, CommunicationResponse](
      _ => CommunicationResponse.notHandled(InternalCommunicationError("Test")),
      ignore = _.message.isDisconnect
    )

  def devNullPacketDispatcher[F[_]: Monad: Temporal]: Dispatcher[F, Blob, Unit] =
    new Dispatcher[F, Blob, Unit](response = kp(()))
}
