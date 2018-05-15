package coop.rchain.node

import java.io.File
import java.util.UUID
import io.grpc.{Server, ServerBuilder}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.comm._, CommError._
import coop.rchain.p2p
import coop.rchain.p2p.Network.KeysStore
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import monix.eval.Task
import monix.execution.Scheduler

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
//  TODO: figure out actual validator identities...
    com.google.protobuf.ByteString.copyFrom(Array((scala.util.Random.nextInt(10) + 1).toByte))
  )

  implicit val packetHandlerEffect: PacketHandler[Effect] = effects.packetHandler[Effect](
    casperPacketHandler[Effect]
  )

  case class Resources(grpcServer: Server,
                       metricsServer: MetricsServer,
                       httpServer: HttpServer,
                       runtime: Runtime)

  def aquireResources(implicit scheduler: Scheduler): Effect[Resources] =
    for {
      runtime <- Runtime.create(conf.data_dir().resolve("rspace"), conf.map_size()).pure[Effect]
      grpcServer <- GrpcServer
                     .acquireServer[Effect](conf.grpcPort(), runtime)
      metricsServer <- MetricsServer.create[Effect](conf.metricsPort())
      httpServer    <- HttpServer(conf.httpPort()).pure[Effect]
    } yield Resources(grpcServer, metricsServer, httpServer, runtime)

  def startResources(resources: Resources): Effect[Unit] =
    for {
      _ <- resources.httpServer.start.toEffect
      _ <- resources.metricsServer.start.toEffect
      _ <- GrpcServer.start[Effect](resources.grpcServer)
    } yield ()

  def clearResources(resources: Resources): Unit = {
    println("Shutting down gRPC server...")
    resources.grpcServer.shutdown()
    println("Shutting down transport layer, broadcasting DISCONNECT")
    net.broadcast(
      DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
    println("Shutting down metrics server...")
    resources.metricsServer.stop()
    println("Shutting down HTTP server....")
    resources.httpServer.stop()
    println("Shutting down interpreter runtime ...")
    resources.runtime.store.close()

    println("Goodbye.")
  }

  def addShutdownHook(resources: Resources): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(resources)))

  def nodeProgram(implicit scheduler: Scheduler): Effect[Unit] =
    for {
      resources <- aquireResources
      _         <- startResources(resources)
      _         <- addShutdownHook(resources).toEffect
      _         <- Task(MonadOps.forever(net.receiver[Effect].value.void)).executeAsync.start.toEffect
      _         <- Log[Effect].info(s"Listening for traffic on $address.")
      res <- ApplicativeError_[Effect, CommError].attempt(
              if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
              else
                conf.bootstrap.toOption
                  .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                  .toEffect >>= (addr => p2p.Network.connectToBootstrap[Effect](addr)))
      _ <- if (res.isRight) MonadOps.forever(p2p.Network.findAndConnect[Effect], 0)
          else ().pure[Effect]
      _ <- Task.delay(System.exit(0)).toEffect // YEAH, I KNOW, DO BETTER, I DARE YOU
    } yield ()
}
