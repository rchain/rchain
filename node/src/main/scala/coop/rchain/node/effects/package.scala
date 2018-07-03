package coop.rchain.node

import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import scala.tools.jline.console._
import cats._, cats.data._, cats.implicits._, cats.mtl.MonadState
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval._
import monix.execution.atomic._
import monix.execution._
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import java.io.File

package object effects {

  def log: Log[Task] = new Log[Task] {
    import com.typesafe.scalalogging.Logger

    def debug(msg: String)(implicit ev: LogSource): Task[Unit] =
      Task.delay(Logger(ev.clazz).debug(msg))
    def info(msg: String)(implicit ev: LogSource): Task[Unit] =
      Task.delay(Logger(ev.clazz).info(msg))
    def warn(msg: String)(implicit ev: LogSource): Task[Unit] =
      Task.delay(Logger(ev.clazz).warn(msg))
    def error(msg: String)(implicit ev: LogSource): Task[Unit] =
      Task.delay(Logger(ev.clazz).error(msg))
  }

  def time: Time[Task] = new Time[Task] {
    def currentMillis: Task[Long] = Task.delay {
      System.currentTimeMillis
    }
    def nanoTime: Task[Long] = Task.delay {
      System.nanoTime
    }
  }

  def ping[F[_]: Monad: Capture: Metrics: TransportLayer](src: PeerNode,
                                                          timeout: FiniteDuration): Ping[F] =
    new Ping[F] {
      import scala.concurrent.duration._
      def ping(node: PeerNode): F[Boolean] =
        for {
          _   <- Metrics[F].incrementCounter("protocol-ping-sends")
          req = ProtocolHelper.ping(src)
          res <- TransportLayer[F].roundTrip(node, req, timeout)
        } yield res.toOption.isDefined
    }

  def tcpTranposrtLayer(host: String, port: Int, cert: File, key: File)(src: PeerNode)(
      implicit scheduler: Scheduler,
      connections: TcpTransportLayer.State,
      log: Log[Task]) =
    new TcpTransportLayer(host, port, cert, key)(src)

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)

  def connectionsState[F[_]: Monad: Capture]: MonadState[F, TransportState] = {
    val state = AtomicAny(TransportState())
    new AtomicMonadState[F, TransportState](state)
  }
}
