package coop.rchain.node

import coop.rchain.comm._
import coop.rchain.metrics.Metrics

import scala.tools.jline.console._
import cats._, cats.data._, cats.implicits._, cats.mtl._, cats.effect.{Sync, Timer}
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval._
import monix.execution._
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._

import scala.concurrent.duration.FiniteDuration
import java.nio.file.Path
import java.time.{LocalDate, LocalDateTime}

import scala.io.Source
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp._
import Connect._

import scala.concurrent.duration._

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

  def dateTimeFromSync[F[_]: Sync]: DateTime[F] = new DateTime[F] {
    override def dateTime: F[LocalDateTime] = Sync[F].delay(LocalDateTime.now())
    override def date: F[LocalDate]         = Sync[F].delay(LocalDate.now())
  }

  def kademliaRPC(src: PeerNode, port: Int, timeout: FiniteDuration)(
      implicit
      scheduler: Scheduler,
      metrics: Metrics[Task],
      log: Log[Task]
  ): KademliaRPC[Task] = new GrpcKademliaRPC(src, port, timeout)

  def tcpTransportLayer(
      host: String,
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
    new TcpTransportLayer(host, port, cert, key, maxMessageSize)
  }

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def tcpConnections: Task[Cell[Task, TransportState]] = Cell.mvarCell(TransportState.empty)

  def rpConnections: Task[ConnectionsCell[Task]] =
    Cell.mvarCell[Connections](Connections.empty)

  def rpConfAsk(conf: RPConf): ApplicativeAsk[Task, RPConf] =
    new ConstApplicativeAsk[Task, RPConf](conf)
}
