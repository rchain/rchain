package coop.rchain.comm.discovery

import java.net.ServerSocket

import scala.collection.mutable
import scala.concurrent.duration._

import cats._
import cats.effect.Timer
import cats.syntax.all._

import coop.rchain.comm._
import coop.rchain.grpc.Server
import coop.rchain.shared.Resources

abstract class KademliaRPCRuntime[F[_]: Monad: Timer, E <: Environment] {

  def createEnvironment(port: Int): F[E]

  def createKademliaRPC(env: E): F[KademliaRPC[F]]
  def createKademliaRPCServer(
      env: E,
      pingHandler: PeerNode => F[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]]
  ): F[Server[F]]

  def extract[A](fa: F[A]): A

  private def getFreePort: Int =
    Resources.withResource(new ServerSocket(0)) { s =>
      s.setReuseAddress(true)
      s.getLocalPort
    }

  def twoNodesEnvironment[A](block: (E, E) => F[A]): F[A] =
    for {
      e1 <- createEnvironment(getFreePort)
      e2 <- createEnvironment(getFreePort)
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
            local    <- e1.peer.pure[F]
            remote   = e2.peer
            localRpc <- createKademliaRPC(e1)
            remoteRpc <- createKademliaRPCServer(
                          e2,
                          pingHandler.handle(remote),
                          lookupHandler.handle(remote)
                        )
            _ <- remoteRpc.start
            r <- execute(localRpc, local, remote)
            _ <- remoteRpc.stop
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
            r        <- execute(localRpc, local, remote)
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

}

trait Environment {
  def peer: PeerNode
  def host: String
  def port: Int
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
        _ <- receivedMessages.synchronized(receivedMessages += ((peer, (p, a)))).pure[F]
        _ <- delay.fold(().pure[F])(implicitly[Timer[F]].sleep)
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
