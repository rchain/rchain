package coop.rchain.comm.transport

import scala.concurrent.duration.Duration

import coop.rchain.shared
import coop.rchain.comm.PeerNode
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.{Cell, Log}

import monix.eval.Task
import monix.execution.Scheduler

class TcpTransportLayerSpec extends TransportLayerSpec[Task, TcpTlsEnvironment] {

  implicit val log: Log[Task]       = new shared.Log.NOPLog[Task]
  implicit val scheduler: Scheduler = Scheduler.Implicits.global

  def createEnvironment(port: Int): Task[TcpTlsEnvironment] =
    Task.delay {
      val host    = "127.0.0.1"
      val keyPair = CertificateHelper.generateKeyPair(true)
      val cert    = CertificatePrinter.print(CertificateHelper.generate(keyPair))
      val key     = CertificatePrinter.printPrivateKey(keyPair.getPrivate)
      val id      = CertificateHelper.publicAddress(keyPair.getPublic).map(Base16.encode).get
      val address = s"rnode://$id@$host:$port"
      val peer    = PeerNode.parse(address).right.get
      TcpTlsEnvironment(host, port, cert, key, peer)
    }

  def createTransportLayer(env: TcpTlsEnvironment): Task[TransportLayer[Task]] =
    Cell.mvarCell(TransportState.empty).map { cell =>
      new TcpTransportLayer(env.host, env.port, env.cert, env.key, 4 * 1024 * 1024)(scheduler,
                                                                                    cell,
                                                                                    log)
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
