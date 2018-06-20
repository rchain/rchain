package coop.rchain.node

import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p
import coop.rchain.p2p.effects._
import coop.rchain.comm._, CommError._
import coop.rchain.metrics.Metrics
import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Path}
import scala.tools.jline.console._, completer.StringsCompleter
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval.Task
import scala.concurrent.ExecutionContext
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._

package object effects {

  def log: Log[Task] = new Log[Task] {
    import com.typesafe.scalalogging.Logger

    val logger = Logger("logger")

    def debug(msg: String): Task[Unit] = Task.delay(logger.debug(msg))
    def info(msg: String): Task[Unit]  = Task.delay(logger.info(msg))
    def warn(msg: String): Task[Unit]  = Task.delay(logger.warn(msg))
    def error(msg: String): Task[Unit] = Task.delay(logger.error(msg))
  }

  def time: Time[Task] = new Time[Task] {
    def currentMillis: Task[Long] = Task.delay {
      System.currentTimeMillis
    }
    def nanoTime: Task[Long] = Task.delay {
      System.nanoTime
    }
  }

  def ping[F[_]: Monad: Capture: Metrics: TransportLayer](src: PeerNode): Ping[F] =
    new Ping[F] {
      import scala.concurrent.duration._
      def ping(node: PeerNode): F[Boolean] =
        for {
          _   <- Metrics[F].incrementCounter("protocol-ping-sends")
          req = PingMessage(ProtocolMessage.ping(src), System.currentTimeMillis)
          res <- TransportLayer[F].roundTrip(req, node, 500.milliseconds).map(_.toOption)
        } yield res.isDefined
    }

  def tcpTranposrtLayer[F[_]: Monad: Capture: Metrics: Futurable](conf: Conf)(src: PeerNode)(
      implicit executionContext: ExecutionContext) =
    new TcpTransportLayer[F](conf.run.localhost,
                             conf.run.port(),
                             conf.run.certificatePath.toFile,
                             conf.run.keyPath.toFile)(src)

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)
}
