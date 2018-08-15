package coop.rchain.comm.transport

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.concurrent.duration._

import cats._
import cats.implicits._

import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.CommError.CommErr

abstract class TransportLayerRuntime[F[_]: Monad, E <: Environment] {

  def createEnvironment(port: Int): F[E]

  def createTransportLayer(env: E): F[TransportLayer[F]]

  def extract[A](fa: F[A]): A

  private val nextPort = new AtomicInteger(41000)

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
    protected def dispatcher: Dispatcher[F]
    def run(): Result
    def await(): Unit = dispatcher.await()

    trait Result {
      def localNode: PeerNode
      def apply(): A
      def receivedMessages: Seq[(PeerNode, Protocol)] = dispatcher.received
      def lastProcessedMessageTimestamp: Long         = dispatcher.lastProcessedTimestamp
    }
  }

  abstract class TwoNodesRuntime[A](val dispatcher: Dispatcher[F]) extends Runtime[A] {
    def execute(transportLayer: TransportLayer[F], local: PeerNode, remote: PeerNode): F[A]

    def run(): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            localTl  <- createTransportLayer(e1)
            remoteTl <- createTransportLayer(e2)
            local    = e1.peer
            remote   = e2.peer
            _        <- remoteTl.receive(dispatcher.dispatch(remote))
            r        <- execute(localTl, local, remote)
            _        <- remoteTl.shutdown(CommMessages.disconnect(remote))
            _        <- localTl.shutdown(CommMessages.disconnect(local))
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

  abstract class TwoNodesRemoteDeadRuntime[A](val dispatcher: Dispatcher[F]) extends Runtime[A] {
    def execute(transportLayer: TransportLayer[F], local: PeerNode, remote: PeerNode): F[A]

    def run(): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            localTl <- createTransportLayer(e1)
            local   = e1.peer
            remote  = e2.peer
            r       <- execute(localTl, local, remote)
            _       <- localTl.shutdown(CommMessages.disconnect(local))
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

  abstract class ThreeNodesRuntime[A](val dispatcher: Dispatcher[F]) extends Runtime[A] {
    def execute(transportLayer: TransportLayer[F],
                local: PeerNode,
                remote1: PeerNode,
                remote2: PeerNode): F[A]

    def run(): ThreeNodesResult =
      extract(
        threeNodesEnvironment { (e1, e2, e3) =>
          for {
            localTl   <- createTransportLayer(e1)
            remoteTl1 <- createTransportLayer(e2)
            remoteTl2 <- createTransportLayer(e3)
            local     = e1.peer
            remote1   = e2.peer
            remote2   = e3.peer
            _         <- remoteTl1.receive(dispatcher.dispatch(remote1))
            _         <- remoteTl2.receive(dispatcher.dispatch(remote2))
            r         <- execute(localTl, local, remote1, remote2)
            _         <- remoteTl1.shutdown(CommMessages.disconnect(remote1))
            _         <- remoteTl2.shutdown(CommMessages.disconnect(remote2))
            _         <- localTl.shutdown(CommMessages.disconnect(local))
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

  def roundTripWithPing(transportLayer: TransportLayer[F],
                        local: PeerNode,
                        remote: PeerNode,
                        timeout: FiniteDuration = 3.second): F[CommErr[Protocol]] =
    transportLayer.roundTrip(remote, ProtocolHelper.ping(local), timeout)

  def sendPing(transportLayer: TransportLayer[F], local: PeerNode, remote: PeerNode): F[Unit] =
    transportLayer.send(remote, ProtocolHelper.ping(local))

  def broadcastPing(transportLayer: TransportLayer[F],
                    local: PeerNode,
                    remotes: PeerNode*): F[Unit] =
    transportLayer.broadcast(remotes, ProtocolHelper.ping(local))

}

trait Environment {
  def peer: PeerNode
  def host: String
  def port: Int
}

final class Dispatcher[F[_]: Applicative](
    response: PeerNode => CommunicationResponse,
    latch: Option[java.util.concurrent.CountDownLatch] = None,
    delay: Option[Long] = None
) {
  def dispatch(peer: PeerNode): Protocol => F[CommunicationResponse] =
    p => {
      processed = System.currentTimeMillis()
      latch.foreach(_.countDown())
      delay.foreach(Thread.sleep)
      val isDisconnect =
        p.message.isUpstream && p.message.upstream.get.typeUrl == "type.googleapis.com/coop.rchain.comm.protocol.rchain.Disconnect"
      // Ignore Disconnect messages to not skew the tests
      if (!isDisconnect)
        receivedMessages.synchronized(receivedMessages += ((peer, p)))
      response(peer).pure[F]
    }
  def received: Seq[(PeerNode, Protocol)] = receivedMessages
  def lastProcessedTimestamp: Long        = processed
  def await(): Unit                       = latch.foreach(_.await(2, TimeUnit.SECONDS))
  private val receivedMessages            = mutable.MutableList.empty[(PeerNode, Protocol)]
  private var processed                   = 0L
}

object Dispatcher {
  def pongDispatcher[F[_]: Applicative]: Dispatcher[F] =
    new Dispatcher(peer => CommunicationResponse.handledWithMessage(ProtocolHelper.pong(peer)))

  def pongDispatcherWithDelay[F[_]: Applicative](delay: Long): Dispatcher[F] =
    new Dispatcher(
      peer => CommunicationResponse.handledWithMessage(ProtocolHelper.pong(peer)),
      delay = Some(delay)
    )

  def dispatcherWithLatch[F[_]: Applicative](countDown: Int = 1): Dispatcher[F] =
    new Dispatcher(
      _ => CommunicationResponse.handledWithoutMessage,
      latch = Some(new java.util.concurrent.CountDownLatch(countDown))
    )

  def withoutMessageDispatcher[F[_]: Applicative]: Dispatcher[F] =
    new Dispatcher(_ => CommunicationResponse.handledWithoutMessage)

  def internalCommunicationErrorDispatcher[F[_]: Applicative]: Dispatcher[F] =
    new Dispatcher(_ => CommunicationResponse.notHandled(InternalCommunicationError("Test")))

}
