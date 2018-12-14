package coop.rchain.comm.discovery

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Random

import cats._
import cats.effect.Timer
import cats.effect.concurrent.MVar
import cats.implicits._

import coop.rchain.comm._

abstract class KademliaRPCRuntime[F[_]: Monad: Timer, E <: Environment] {

  def createEnvironment(port: Int): F[E]

  def createKademliaRPC(env: E): F[KademliaRPC[F]]

  def extract[A](fa: F[A]): A

  private val nextPort = new AtomicInteger((Random.nextInt(100) + 500 + 1) * 100)

  def twoNodesEnvironment[A](block: (E, E) => F[A]): F[A] =
    for {
      e1 <- createEnvironment(nextPort.incrementAndGet())
      e2 <- createEnvironment(nextPort.incrementAndGet())
      r  <- block(e1, e2)
    } yield r

  trait Runtime[A] {
    protected def pingHandler: PingHandler[F]
    protected def lookupHandler: LookupHandler[F]
    def run(): Result
    trait Result {
      def localNode: PeerNode
      def apply(): A
    }
  }

  abstract class TwoNodesRuntime[A](
      val pingHandler: PingHandler[F] = Handler.pingHandler,
      val lookupHandler: LookupHandler[F] = Handler.lookupHandlerNil
  ) extends Runtime[A] {
    def execute(kademliaRPC: KademliaRPC[F], local: PeerNode, remote: PeerNode): F[A]

    def run(): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            localRpc  <- createKademliaRPC(e1)
            remoteRpc <- createKademliaRPC(e2)
            local     = e1.peer
            remote    = e2.peer
            _ <- localRpc.receive(
                  Handler.pingHandler[F].handle(local),
                  Handler.lookupHandlerNil[F].handle(local)
                )
            _ <- remoteRpc.receive(
                  pingHandler.handle(remote),
                  lookupHandler.handle(remote)
                )
            r <- execute(localRpc, local, remote)
            _ <- remoteRpc.shutdown()
            _ <- localRpc.shutdown()
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

  abstract class TwoNodesRemoteDeadRuntime[A](
      val pingHandler: PingHandler[F] = Handler.pingHandler,
      val lookupHandler: LookupHandler[F] = Handler.lookupHandlerNil
  ) extends Runtime[A] {
    def execute(kademliaRPC: KademliaRPC[F], local: PeerNode, remote: PeerNode): F[A]

    def run(): TwoNodesResult =
      extract(
        twoNodesEnvironment { (e1, e2) =>
          for {
            localRpc <- createKademliaRPC(e1)
            local    = e1.peer
            remote   = e2.peer
            _ <- localRpc.receive(
                  Handler.pingHandler[F].handle(local),
                  Handler.lookupHandlerNil[F].handle(local)
                )
            r <- execute(localRpc, local, remote)
            _ <- localRpc.shutdown()
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

abstract class Handler[F[_]: Monad: Timer, R] {
  def received: Seq[(PeerNode, R)] = receivedMessages
  protected val receivedMessages: mutable.MutableList[(PeerNode, R)] =
    mutable.MutableList.empty[(PeerNode, R)]
}

final class PingHandler[F[_]: Monad: Timer](
    delay: Option[FiniteDuration] = None
) extends Handler[F, PeerNode] {
  def handle(peer: PeerNode): PeerNode => F[Unit] =
    p =>
      for {
        _ <- receivedMessages.synchronized(receivedMessages += ((peer, p))).pure[F]
        _ <- delay.fold(().pure[F])(implicitly[Timer[F]].sleep)
      } yield ()
}

final class LookupHandler[F[_]: Monad: Timer](
    response: Seq[PeerNode],
    delay: Option[FiniteDuration] = None
) extends Handler[F, (PeerNode, Array[Byte])] {
  def handle(peer: PeerNode): (PeerNode, Array[Byte]) => F[Seq[PeerNode]] =
    (p, a) =>
      for {
        _ <- delay.fold(().pure[F])(implicitly[Timer[F]].sleep)
        _ <- receivedMessages.synchronized(receivedMessages += ((peer, (p, a)))).pure[F]
      } yield response
}

object Handler {
  def pingHandler[F[_]: Monad: Timer]: PingHandler[F] = new PingHandler[F]

  def pingHandlerWithDelay[F[_]: Monad: Timer](delay: FiniteDuration): PingHandler[F] =
    new PingHandler[F](Some(delay))

  def lookupHandlerNil[F[_]: Monad: Timer]: LookupHandler[F] = new LookupHandler[F](Nil)

  def lookupHandlerWithDelay[F[_]: Monad: Timer](delay: FiniteDuration): LookupHandler[F] =
    new LookupHandler[F](Nil, Some(delay))

  def lookupHandler[F[_]: Monad: Timer](result: Seq[PeerNode]): LookupHandler[F] =
    new LookupHandler[F](result)
}
