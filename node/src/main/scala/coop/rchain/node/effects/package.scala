package coop.rchain.node

import java.nio.file.Path

import cats.data.ReaderT
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Sync, Timer}
import cats.mtl._
import cats.syntax.all._
import cats.{Applicative, Monad, Parallel}
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.shared._
import monix.eval._
import monix.execution._
import monix.execution.atomic.AtomicAny

import scala.concurrent.duration._
import scala.io.Source
import scala.tools.jline.console._

package object effects {

  def log: Log[Task] = Log.log

  def kademliaStore[F[_]: Sync: KademliaRPC: Metrics](id: NodeIdentifier): KademliaStore[F] =
    KademliaStore.table[F](id)

  def nodeDiscovery[F[_]: Monad](id: NodeIdentifier)(
      implicit
      kademliaStore: KademliaStore[F],
      kademliaRPC: KademliaRPC[F]
  ): NodeDiscovery[F] = NodeDiscovery.kademlia(id)

  def time[F[_]](implicit timer: Timer[F]): Time[F] =
    new Time[F] {
      def currentMillis: F[Long]                   = timer.clock.realTime(MILLISECONDS)
      def nanoTime: F[Long]                        = timer.clock.monotonic(NANOSECONDS)
      def sleep(duration: FiniteDuration): F[Unit] = timer.sleep(duration)
    }

  def kademliaRPC[F[_]: Monixable: Sync: PeerNodeAsk: Metrics](
      networkId: String,
      timeout: FiniteDuration,
      allowPrivateAddresses: Boolean
  )(implicit scheduler: Scheduler): KademliaRPC[F] =
    new GrpcKademliaRPC(networkId, timeout, allowPrivateAddresses)

  def transportClient[F[_]: Monixable: Concurrent: Parallel: Log: Metrics](
      networkId: String,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      packetChunkSize: Int,
      networkTimeout: FiniteDuration,
      ioScheduler: Scheduler
  )(implicit scheduler: Scheduler): F[TransportLayer[F]] =
    Ref.of[F, Map[PeerNode, Deferred[F, BufferedGrpcStreamChannel[F]]]](Map()) map { channels =>
      val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
      val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
      new GrpcTransportClient(
        networkId,
        cert,
        key,
        maxMessageSize,
        packetChunkSize,
        clientQueueSize = 100,
        networkTimeout,
        channels,
        ioScheduler
      ): TransportLayer[F]
    }

  def consoleIO[F[_]: Sync](consoleReader: ConsoleReader): ConsoleIO[F] =
    new JLineConsoleIO(consoleReader)

  def rpConnections[F[_]: Concurrent]: F[ConnectionsCell[F]] =
    Cell.mvarCell[F, Connections](Connections.empty)

  def rpConfState[F[_]: Monad: Sync](conf: RPConf): MonadState[F, RPConf] =
    new AtomicMonadState[F, RPConf](AtomicAny(conf))

  def rpConfAsk[F[_]: Monad](
      implicit state: MonadState[F, RPConf]
  ): ApplicativeAsk[F, RPConf] =
    new DefaultApplicativeAsk[F, RPConf] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[RPConf]              = state.get
    }

  def peerNodeAsk[F[_]: Monad](
      implicit state: MonadState[F, RPConf]
  ): ApplicativeAsk[F, PeerNode] =
    new DefaultApplicativeAsk[F, PeerNode] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[PeerNode]            = state.get.map(_.local)
    }

  def readerTApplicativeAsk[F[_]: Monad, E, B](
      askF: ApplicativeAsk[F, B]
  ): ApplicativeAsk[ReaderT[F, E, ?], B] =
    new ApplicativeAsk[ReaderT[F, E, ?], B] {
      override val applicative: Applicative[ReaderT[F, E, ?]] = Applicative[ReaderT[F, E, ?]]

      override def ask: ReaderT[F, E, B] = ReaderT.liftF(askF.ask)

      override def reader[A](f: B => A): ReaderT[F, E, A] = ReaderT.liftF(askF.reader(f))
    }

  def readerTMonadState[F[_]: Monad: Sync, E, S](
      stateF: MonadState[F, S]
  ): MonadState[ReaderT[F, E, ?], S] =
    new MonadState[ReaderT[F, E, ?], S] {
      override val monad: Monad[ReaderT[F, E, ?]] = Monad[ReaderT[F, E, ?]]

      override def get: ReaderT[F, E, S] = ReaderT.liftF(stateF.get)

      override def set(s: S): ReaderT[F, E, Unit] = ReaderT.liftF(stateF.set(s))

      override def inspect[A](f: S => A): ReaderT[F, E, A] = ReaderT.liftF(stateF.inspect(f))

      override def modify(f: S => S): ReaderT[F, E, Unit] = ReaderT.liftF(stateF.modify(f))
    }

}
