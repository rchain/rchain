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

import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.tools.jline.console._

package object effects {

  def log: Log[Task] = Log.log

  def nodeDiscovery(src: PeerNode, defaultTimeout: FiniteDuration)(init: Option[PeerNode])(
      implicit
      log: Log[Task],
      time: Timer[Task],
      metrics: Metrics[Task],
      kademliaRPC: KademliaRPC[Task]
  ): Task[NodeDiscovery[Task]] =
    KademliaNodeDiscovery.create[Task](src, defaultTimeout)(init)

  def time: Time[Task] = new Time[Task] {
    def currentMillis: Task[Long] = Task.delay {
      System.currentTimeMillis
    }
    def nanoTime: Task[Long] = Task.delay {
      System.nanoTime
    }

    def sleep(millis: Int): Task[Unit] = Task.delay {
      Thread.sleep(millis.toLong)
    }
  }

  def kademliaRPC(src: PeerNode, port: Int, timeout: FiniteDuration)(
      implicit
      scheduler: Scheduler,
      metrics: Metrics[Task],
      log: Log[Task],
      cache: ConnectionsCache[Task, KademliaConnTag]
  ): KademliaRPC[Task] = new GrpcKademliaRPC(src, port, timeout)

  def tcpTransportLayer(
      host: String,
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int
  )(
      implicit scheduler: Scheduler,
      log: Log[Task],
      cache: ConnectionsCache[Task, TcpConnTag]
  ): TcpTransportLayer = {
    val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
    new TcpTransportLayer(host, port, cert, key, maxMessageSize)
  }

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def rpConnections: Task[ConnectionsCell[Task]] =
    Cell.mvarCell[Task, Connections](Connections.empty)

  def rpConfAsk(conf: RPConf): ApplicativeAsk[Task, RPConf] =
    new ConstApplicativeAsk[Task, RPConf](conf)
}
