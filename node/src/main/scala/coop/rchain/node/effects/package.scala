package coop.rchain.node

import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import scala.tools.jline.console._
import cats._, cats.data._, cats.implicits._, cats.mtl._, cats.effect.Timer
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval._
import monix.execution._
import monix.execution.atomic.AtomicAny
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import scala.concurrent.duration._
import java.nio.file.Path
import scala.io.Source
import coop.rchain.comm.rp._, Connect._

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
      log: Log[Task]
  ): KademliaRPC[Task] = new GrpcKademliaRPC(port, timeout)

  def tcpTransportLayer(
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int
  )(
      implicit scheduler: Scheduler,
      connections: TcpTransportLayer.TransportCell[Task],
      log: Log[Task]
  ): TcpTransportLayer = {
    val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
    new TcpTransportLayer(port, cert, key, maxMessageSize)
  }

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def tcpConnections: Task[Cell[Task, TransportState]] = Cell.mvarCell(TransportState.empty)

  def rpConnections: Task[ConnectionsCell[Task]] =
    Cell.mvarCell[Connections](Connections.empty)

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
