package coop.rchain.comm.transport

import scala.concurrent.duration.Duration
import cats._, cats.data._, cats.implicits._, cats.mtl._, cats.effect.Timer
import coop.rchain.shared._
import coop.rchain.comm.PeerNode
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.{Cell, Log}
import scala.concurrent.duration._
import monix.eval.Task
import monix.execution.Scheduler

class TcpTransportLayerSpec extends TransportLayerSpec[Task, TcpTlsEnvironment] {

  implicit val log: Log[Task]       = new Log.NOPLog[Task]
  implicit val scheduler: Scheduler = Scheduler.Implicits.global

  def timer: Timer[Task] = implicitly[Timer[Task]]

  def time: Time[Task] =
    new Time[Task] {
      def currentMillis: Task[Long]                   = timer.clock.realTime(MILLISECONDS)
      def nanoTime: Task[Long]                        = timer.clock.monotonic(NANOSECONDS)
      def sleep(duration: FiniteDuration): Task[Unit] = timer.sleep(duration)
    }

  def createEnvironment(port: Int): Task[TcpTlsEnvironment] =
    Task.delay {
      val host    = "127.0.0.1"
      val keyPair = CertificateHelper.generateKeyPair(true)
      val cert    = CertificatePrinter.print(CertificateHelper.generate(keyPair))
      val key     = CertificatePrinter.printPrivateKey(keyPair.getPrivate)
      val id      = CertificateHelper.publicAddress(keyPair.getPublic).map(Base16.encode).get
      val address = s"rnode://$id@$host?protocol=$port&discovery=0"
      val peer    = PeerNode.fromAddress(address).right.get
      TcpTlsEnvironment(host, port, cert, key, peer)
    }

  def maxMessageSize: Int = 4 * 1024 * 1024

  def createTransportLayer(env: TcpTlsEnvironment): Task[TransportLayer[Task]] =
    Cell.mvarCell(TransportState.empty).map { cell =>
      new TcpTransportLayer(env.host, env.port, env.cert, env.key, maxMessageSize)(
        scheduler,
        cell,
        log
      )
    }

  def extract[A](fa: Task[A]): A = fa.runSyncUnsafe(Duration.Inf)
}

case class TcpTlsEnvironment(
    host: String,
    port: Int,
    cert: String,
    key: String,
    peer: PeerNode
) extends Environment
