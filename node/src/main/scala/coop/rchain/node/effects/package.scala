package coop.rchain.node

import java.nio.file.Path

import cats.data.ReaderT
import cats.effect.concurrent.{Deferred, Ref}

import scala.concurrent.duration._
import scala.io.Source
import scala.tools.jline.console._
import cats.effect.{Concurrent, Sync, Timer}
import cats.mtl._
import cats.implicits._
import cats.{Applicative, Monad}
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp._
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.shared._
import io.grpc.ManagedChannel
import monix.eval._
import monix.execution._
import monix.execution.atomic.AtomicAny

package object effects {

  def log: Log[Task] = Log.log

  def kademliaStore(id: NodeIdentifier)(
      implicit
      kademliaRPC: KademliaRPC[Task],
      metrics: Metrics[Task]
  ): KademliaStore[Task] = KademliaStore.table(id)

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

  def kademliaRPC(networkId: String, timeout: FiniteDuration, allowPrivateAddresses: Boolean)(
      implicit
      scheduler: Scheduler,
      peerNodeAsk: PeerNodeAsk[Task],
      metrics: Metrics[Task]
  ): KademliaRPC[Task] = new GrpcKademliaRPC(networkId, timeout, allowPrivateAddresses)

  def transportClient(
      networkId: String,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      packetChunkSize: Int,
      folder: Path,
      channels: Ref[Task, Map[PeerNode, Deferred[Task, BufferedGrpcStreamChannel]]]
  )(
      implicit scheduler: Scheduler,
      log: Log[Task],
      metrics: Metrics[Task]
  ): Task[TransportLayer[Task]] =
    Task.delay {
      val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
      val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
      new GrpcTransportClient(
        networkId,
        cert,
        key,
        maxMessageSize,
        packetChunkSize,
        folder,
        100,
        channels
      )
    }

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def rpConnections[F[_]: Concurrent]: F[ConnectionsCell[F]] =
    Cell.mvarCell[F, Connections](Connections.empty)

  def rpConfState[F[_]: Monad: Sync](conf: RPConf): MonadState[F, RPConf] =
    new AtomicMonadState[F, RPConf](AtomicAny(conf))

  def rpConfAsk[F[_]: Monad: Sync](
      implicit state: MonadState[F, RPConf]
  ): ApplicativeAsk[F, RPConf] =
    new DefaultApplicativeAsk[F, RPConf] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[RPConf]              = state.get
    }

  def peerNodeAsk[F[_]: Monad: Sync](
      implicit state: MonadState[F, RPConf]
  ): ApplicativeAsk[F, PeerNode] =
    new DefaultApplicativeAsk[F, PeerNode] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[PeerNode]            = state.get.map(_.local)
    }

  def readerTApplicativeAsk[F[_]: Monad: Sync, E, B](
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
