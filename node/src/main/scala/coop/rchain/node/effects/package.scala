package coop.rchain.node

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Sync}
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

import java.nio.file.Path
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

  def kademliaRPC[F[_]: Monixable: Sync: PeerNodeAsk: Metrics](
      networkId: String,
      timeout: FiniteDuration
  )(implicit scheduler: Scheduler): KademliaRPC[F] = new GrpcKademliaRPC(networkId, timeout)

  def transportClient[F[_]: Monixable: Concurrent: Parallel: Log: Metrics](
      networkId: String,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      packetChunkSize: Int,
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
        channels,
        ioScheduler
      ): TransportLayer[F]
    }

  def consoleIO[F[_]: Sync](consoleReader: ConsoleReader): ConsoleIO[F] =
    new JLineConsoleIO(consoleReader)

  def rpConnections[F[_]: Concurrent]: F[ConnectionsCell[F]] =
    Ref[F].of(Connections.empty)

  def rpConfState[F[_]: Monad: Sync](conf: RPConf): MonadState[F, RPConf] =
    new AtomicMonadState[F, RPConf](AtomicAny(conf))

  def rpConfAsk[F[_]: Monad](state: MonadState[F, RPConf]): ApplicativeAsk[F, RPConf] =
    new DefaultApplicativeAsk[F, RPConf] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[RPConf]              = state.get
    }

  def peerNodeAsk[F[_]: Monad](state: MonadState[F, RPConf]): ApplicativeAsk[F, PeerNode] =
    new DefaultApplicativeAsk[F, PeerNode] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[PeerNode]            = state.get.map(_.local)
    }
}
