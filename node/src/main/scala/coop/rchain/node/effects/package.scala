package coop.rchain.node

import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import scala.tools.jline.console._
import cats._, cats.data._, cats.implicits._, cats.mtl._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval._
import monix.execution.atomic._
import monix.execution._
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import scala.concurrent.duration.FiniteDuration
import java.io.File
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp._, Connect._

package object effects {

  def log: Log[Task] = Log.log

  def nodeDiscovery(src: PeerNode, defaultTimeout: FiniteDuration)(
      implicit
      log: Log[Task],
      time: Time[Task],
      metrics: Metrics[Task],
      transport: TransportLayer[Task],
      kademliaRPC: KademliaRPC[Task]
  ): Task[NodeDiscovery[Task]] =
    KademliaNodeDiscovery.create[Task, Task](src, defaultTimeout)

  def time: Time[Task] = new Time[Task] {
    def currentMillis: Task[Long] = Task.delay {
      System.currentTimeMillis
    }
    def nanoTime: Task[Long] = Task.delay {
      System.nanoTime
    }
  }

  def kademliaRPC(src: PeerNode, timeout: FiniteDuration)(
      implicit
      metrics: Metrics[Task],
      transport: TransportLayer[Task]): KademliaRPC[Task] =
    new KademliaRPC[Task] {
      def ping(node: PeerNode): Task[Boolean] =
        for {
          _   <- Metrics[Task].incrementCounter("protocol-ping-sends")
          req = ProtocolHelper.ping(src)
          res <- TransportLayer[Task].roundTrip(node, req, timeout)
        } yield res.toOption.isDefined

      def lookup(key: Seq[Byte], remoteNode: PeerNode): Task[Seq[PeerNode]] =
        for {
          _   <- Metrics[Task].incrementCounter("protocol-lookup-send")
          req = ProtocolHelper.lookup(src, key)
          r <- TransportLayer[Task]
                .roundTrip(remoteNode, req, timeout)
                .map(_.toOption
                  .map {
                    case Protocol(_, Protocol.Message.LookupResponse(lr)) =>
                      lr.nodes.map(ProtocolHelper.toPeerNode)
                    case _ => Seq()
                  }
                  .getOrElse(Seq()))
        } yield r
    }

  def tcpTransportLayer(host: String, port: Int, cert: File, key: File)(src: PeerNode)(
      implicit scheduler: Scheduler,
      connections: TcpTransportLayer.TransportCell[Task],
      log: Log[Task]) =
    new TcpTransportLayer(host, port, cert, key)(src)

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def tcpConnections: Task[Cell[Task, TransportState]] = Cell.mvarCell(TransportState.empty)
  def rpConnections: Task[ConnectionsCell[Task]] =
    Cell.const[Task, Connections](Connections.empty).pure[Task] // noop for now

  def rpConfAsk(conf: RPConf): ApplicativeAsk[Task, RPConf] =
    new ConstApplicativeAsk[Task, RPConf](conf)
}
