package coop.rchain.comm.transport

import cats.effect.{IO, Sync}
import cats.effect.std.PQueue
import cats.effect.unsafe.implicits.global
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.crypto.util.{CertificateHelper, CertificatePrinter}
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.{Base16, Log}
import cats.effect.{Deferred, Ref}

class TcpTransportLayerSpec extends TransportLayerSpec[IO, TcpTlsEnvironment] {

  implicit val log: Log[IO]         = new Log.NOPLog[IO]
  implicit val metrics: Metrics[IO] = new Metrics.MetricsNOP

  def createEnvironment(port: Int): IO[TcpTlsEnvironment] =
    IO.delay {
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
  ): IO[TransportLayer[IO]] =
    IO.delay(
      new GrpcTransportClient(
        networkId,
        env.cert,
        env.key,
        maxMessageSize,
        maxMessageSize,
        100,
        Ref.unsafe[IO, Map[PeerNode, Deferred[IO, BufferedGrpcStreamChannel[IO]]]](Map.empty)
      )
    )

  def extract[A](fa: IO[A]): A = fa.unsafeRunSync

  def createDispatcherCallback: IO[DispatcherCallback[IO]] =
    PQueue.bounded[IO, Unit](1).map(new DispatcherCallback(_))

  def createTransportLayerServer(env: TcpTlsEnvironment): IO[TransportLayerServer[IO]] =
    IO.delay {
      implicit val rPConfAsk: RPConfAsk[IO] = createRPConfAsk[IO](env.peer)
      val server = new GrpcTransportServer[IO](
        networkId,
        env.port,
        env.cert,
        env.key,
        maxMessageSize,
        maxStreamMessageSize,
        4
      )
      TransportLayerServer(server)
    }
}

case class TcpTlsEnvironment(
    host: String,
    port: Int,
    cert: String,
    key: String,
    peer: PeerNode
) extends Environment
