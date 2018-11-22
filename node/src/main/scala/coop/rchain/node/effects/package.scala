package coop.rchain.node

import java.nio.file.Path

import cats.effect.Timer
import cats.mtl._

import coop.rchain.catscontrib._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.shared._

import monix.eval._
import monix.execution._
import monix.execution.atomic.AtomicAny
import scala.concurrent.duration._
import scala.io.Source
import scala.tools.jline.console._

import cats.Applicative

package object effects {

  def log: Log[Task] = Log.log

  def nodeDiscovery(id: NodeIdentifier, defaultTimeout: FiniteDuration)(init: Option[PeerNode])(
      implicit
      log: Log[Task],
      time: Time[Task],
      metrics: Metrics[Task],
      kademliaRPC: KademliaRPC[Task]
  ): Task[NodeDiscovery[Task]] =
    KademliaNodeDiscovery.create[Task](id, defaultTimeout)(init)

  def time(implicit timer: Timer[Task]): Time[Task] =
    new Time[Task] {
      def currentMillis: Task[Long]                   = timer.clock.realTime(MILLISECONDS)
      def nanoTime: Task[Long]                        = timer.clock.monotonic(NANOSECONDS)
      def sleep(duration: FiniteDuration): Task[Unit] = timer.sleep(duration)
    }

  def kademliaRPC(port: Int, timeout: FiniteDuration)(
      implicit
      scheduler: Scheduler,
      peerNodeAsk: PeerNodeAsk[Task],
      metrics: Metrics[Task],
      log: Log[Task],
      cache: ConnectionsCache[Task, KademliaConnTag]
  ): KademliaRPC[Task] = new GrpcKademliaRPC(port, timeout)

  def tcpTransportLayer(
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      folder: Path
  )(
      implicit scheduler: Scheduler,
      log: Log[Task],
      cache: ConnectionsCache[Task, TcpConnTag]
  ): TcpTransportLayer = {
    val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
    new TcpTransportLayer(port, cert, key, maxMessageSize, folder)
  }

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def rpConnections: Task[ConnectionsCell[Task]] =
    Cell.mvarCell[Task, Connections](Connections.empty)

  def rpConfState(conf: RPConf): MonadState[Task, RPConf] =
    new AtomicMonadState[Task, RPConf](AtomicAny(conf))

  def rpConfAsk(implicit state: MonadState[Task, RPConf]): ApplicativeAsk[Task, RPConf] =
    new DefaultApplicativeAsk[Task, RPConf] {
      val applicative: Applicative[Task] = Applicative[Task]
      def ask: Task[RPConf]              = state.get
    }

  def peerNodeAsk(implicit state: MonadState[Task, RPConf]): ApplicativeAsk[Task, PeerNode] =
    new DefaultApplicativeAsk[Task, PeerNode] {
      val applicative: Applicative[Task] = Applicative[Task]
      def ask: Task[PeerNode]            = state.get.map(_.local)
    }

}
