package coop.rchain.node

import java.io.File
import java.util.UUID

import cats.data._
import cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.comm.CommUtil.{casperPacketHandler, deployService}
import coop.rchain.comm._
import coop.rchain.p2p
import coop.rchain.p2p.Network.KeysStore
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import monix.eval.Task

import scala.concurrent.ExecutionContext

class NodeRuntime(conf: Conf) {

  import ApplicativeError_._

  /** Configuration */
  private val host           = conf.fetchHost()
  private val name           = conf.name.toOption.fold(UUID.randomUUID.toString.replaceAll("-", ""))(id)
  private val address        = s"rnode://$name@$host:${conf.port()}"
  private val src            = p2p.NetworkAddress.parse(address).right.get
  private val remoteKeysPath = conf.data_dir().resolve("keys").resolve(s"${name}-rnode-remote.keys")
  private val keysPath       = conf.data_dir().resolve("keys").resolve(s"${name}-rnode.keys")

  /** Run services */
  /** TODO all services should be defined in terms of `nodeProgram` */
  val metricsServer = MetricsServer()

  val http = HttpServer(conf.httpPort())
  http.start()

  val runtime: Runtime = Runtime.create(conf.data_dir().resolve("rspace"), conf.map_size())

  val grpc = new GrpcServer(ExecutionContext.global, conf.grpcPort(), runtime)
  grpc.start()

  val net = new UnicastNetwork(src, Some(p2p.Network))

  /** Final Effect + helper methods */
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  implicit class EitherEffectOps[A](e: Either[CommError, A]) {
    def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
  }
  implicit class TaskEffectOps[A](t: Task[A]) {
    def toEffect: Effect[A] = t.liftM[CommErrT]
  }

  /** Capabilities for Effect */
  implicit val encryptionEffect: Encryption[Task]           = effects.encryption(keysPath)
  implicit val logEffect: Log[Task]                         = effects.log
  implicit val timeEffect: Time[Task]                       = effects.time
  implicit val metricsEffect: Metrics[Task]                 = effects.metrics
  implicit val inMemoryPeerKeysEffect: KeysStore[Task]      = effects.remoteKeysKvs(remoteKeysPath)
  implicit val nodeDiscoveryEffect: NodeDiscovery[Effect]   = effects.nodeDiscovery[Effect](net)
  implicit val transportLayerEffect: TransportLayer[Effect] = effects.transportLayer[Effect](net)

  implicit val casperEffect: MultiParentCasper[Effect] = MultiParentCasper.hashSetCasper[Effect](
    //TODO: figure out actual validator identities...
    com.google.protobuf.ByteString.copyFrom(Array((scala.util.Random.nextInt(10) + 1).toByte))
  )

  implicit val packetHandlerEffect: PacketHandler[Effect] = effects.packetHandler[Effect](
    casperPacketHandler[Effect]
  )

  def addShutdownHook(): Task[Unit] = Task.delay {
    sys.addShutdownHook {
      runtime.store.close()
      metricsServer.stop()
      http.stop()
      grpc.stop()
      net.broadcast(
        DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
      println("Goodbye.")
    }
  }

  val nodeProgram: Effect[Unit] = for {
    _ <- MonadOps.forever(net.receiver[Effect].value.void).executeAsync.start.toEffect
    _ <- addShutdownHook().toEffect
    _ <- Log[Effect].info(s"Listening for traffic on $address.")
    _ <- if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
        else
          conf.bootstrap.toOption
            .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
            .toEffect >>= (addr => p2p.Network.connectToBootstrap[Effect](addr))
    _ <- MonadOps
          .forever(MultiParentCasper[Effect].sendBlockWhenReady.value.void)
          .executeAsync
          .start
          .toEffect
    _ <- MonadOps
          .forever(deployService[Effect].value.void)
          .executeAsync
          .start
          .toEffect
    _ <- MonadOps.forever(p2p.Network.findAndConnect[Effect], 0)
  } yield ()

}
