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

import java.nio.file.Path
import scala.concurrent.duration._
import scala.io.Source
import scala.tools.jline.console._
import scala.util.Using

package object effects {

  def log: Log[Task] = Log.log

  def kademliaStore[F[_]: Sync: KademliaRPC: Metrics](id: NodeIdentifier): KademliaStore[F] =
    KademliaStore.table[F](id)

  def nodeDiscovery[F[_]: Monad: KademliaStore: KademliaRPC](id: NodeIdentifier): NodeDiscovery[F] =
    NodeDiscovery.kademlia(id)

  def kademliaRPC[F[_]: Monixable: Sync: RPConfAsk: Metrics](
      networkId: String,
      timeout: FiniteDuration
  )(implicit scheduler: Scheduler): KademliaRPC[F] = new GrpcKademliaRPC(networkId, timeout)

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
      val cert = Using.resource(Source.fromFile(certPath.toFile))(_.mkString)
      val key  = Using.resource(Source.fromFile(keyPath.toFile))(_.mkString)
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
    Ref[F].of(Connections.empty)

  def rpConfState[F[_]: Sync](conf: RPConf): F[Ref[F, RPConf]] = Ref.of(conf)

  def rpConfAsk[F[_]: Applicative](state: Ref[F, RPConf]): ApplicativeAsk[F, RPConf] =
    new DefaultApplicativeAsk[F, RPConf] {
      val applicative: Applicative[F] = Applicative[F]
      def ask: F[RPConf]              = state.get
    }
}
