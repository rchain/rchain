package coop.rchain.comm.transport

import cats.effect.concurrent.{Deferred, MVar, Ref}
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.crypto.util.{CertificateHelper, CertificatePrinter}
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.{Base16, Log}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.Duration

class TcpTransportLayerSpec extends TransportLayerSpec[Task, TcpTlsEnvironment] {

  implicit val log: Log[Task]         = new Log.NOPLog[Task]
  implicit val scheduler: Scheduler   = Scheduler.Implicits.global
  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP

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

  val maxMessageSize: Int        = 256 * 1024
  val maxStreamMessageSize: Long = 1024 * 1024 * 200

  def createTransportLayer(
      env: TcpTlsEnvironment
  ): Task[TransportLayer[Task]] =
    Task.delay(
      new GrpcTransportClient(
        networkId,
        env.cert,
        env.key,
        maxMessageSize,
        maxMessageSize,
        100,
        Ref.unsafe[Task, Map[PeerNode, Deferred[Task, BufferedGrpcStreamChannel[Task]]]](Map.empty),
        scheduler
      )
    )

  def extract[A](fa: Task[A]): A = fa.runSyncUnsafe(Duration.Inf)

  def createDispatcherCallback: Task[DispatcherCallback[Task]] =
    MVar.empty[Task, Unit].map(new DispatcherCallback(_))

  def createTransportLayerServer(env: TcpTlsEnvironment): Task[TransportLayerServer[Task]] =
    Task.delay {
      implicit val rPConfAsk: RPConfAsk[Task] = createRPConfAsk[Task](env.peer)
      new GrpcTransportServer(
        networkId,
        env.port,
        env.cert,
        env.key,
        maxMessageSize,
        maxStreamMessageSize,
        4
      )
    }
}

case class TcpTlsEnvironment(
    host: String,
    port: Int,
    cert: String,
    key: String,
    peer: PeerNode
) extends Environment
