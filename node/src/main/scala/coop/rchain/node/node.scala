package coop.rchain.node

import java.io.File
import java.util.UUID
import io.grpc.{Server, ServerBuilder}

import cats.data._
import cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.ski._
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

  val net = new UnicastNetwork(src, Some(p2p.Network))

  type Eff[A] = EitherT[Task, CommError, A]

  /** Final Effect + helper methods */
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  implicit class EitherEffectOps[A](e: Either[CommError, A]) {
    def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
  }
  implicit class TaskEffectOps[A](t: Task[A]) {
    def toEffect: Effect[A] = t.liftM[CommErrT]
  }

  case class CommException(error: CommError) extends Exception

  implicit class UnattemptOps[A](t: Task[CommErr[A]]) {
    def unattempt: Task[A] =
      t.map(_ match {
        case Right(a)    => a
        case Left(error) => throw new CommException(error)
      })
  }

  implicit class AttemptOps[A](t: Task[A]) {
    def attemptCE: Task[CommErr[A]] =
      t.attempt.map(_ match {
        case Right(a)                   => Right(a)
        case Left(CommException(error)) => Left(error)
        case Left(exception)            => throw exception
      })
  }

  /** Capabilities for Effect */
  implicit val encryptionEffect: Encryption[Task]           = effects.encryption(keysPath)
  implicit val logEffect: Log[Task]                         = effects.log
  implicit val timeEffect: Time[Task]                       = effects.time
  implicit val metricsEffect: Metrics[Task]                 = effects.metrics
  implicit val inMemoryPeerKeysEffect: KeysStore[Task]      = effects.remoteKeysKvs(remoteKeysPath)
  implicit val nodeDiscoveryEffect: NodeDiscovery[Effect]   = effects.nodeDiscovery[Effect](net)
  implicit val transportLayerEffect: TransportLayer[Effect] = effects.transportLayer[Effect](net)
  implicit val casperEffect: MultiParentCasper[Effect]      = MultiParentCasper.simpleCasper[Effect]
  implicit val packetHandlerEffect: PacketHandler[Effect] = effects.packetHandler[Effect](
    casperPacketHandler[Effect]
  )

  case class Resources(grpcServer: Server)

  def aquireResources(implicit scheduler: Scheduler): Effect[Resources] =
    GrpcServer
      .acquireServer[Effect](ExecutionContext.global, conf.grpcPort(), runtime)
      .map(Resources(_))

  def addShutdownHook(resources: Resources): Task[Unit] =
    Task
      .delay {
        // THIS IS A SMELL
        sys.addShutdownHook {
          println("Shutting down gRPC server...")
          resources.grpcServer.shutdown()
          println("Shutting down interpreter runtime ...")
          runtime.store.close()
          println("Shutting down metrics server...")
          metricsServer.stop
          println("Shutting down HTTP server....")
          http.stop
          println("Shutting down transport layer, broadcasting DISCONNECT")
          net.broadcast(
            DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
          println("Goodbye.")
        }
      }

  def nodeProgram(implicit scheduler: Scheduler): Effect[Unit] =
    for {
      resources <- aquireResources
      _         <- GrpcServer.start[Effect](resources.grpcServer)
      _         <- Task.fork(MonadOps.forever(net.receiver[Effect].value.void)).start.toEffect
      _         <- addShutdownHook(resources).toEffect
      _         <- Log[Effect].info(s"Listening for traffic on $address.")
      _ <- if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
          else
            conf.bootstrap.toOption
              .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
              .toEffect >>= (addr => p2p.Network.connectToBootstrap[Effect](addr))
      _ <- MonadOps
            .forever(MultiParentCasper[Effect].shouldSendBlock.value.void)
            .executeAsync
            .start
            .toEffect
      _ <- MonadOps.forever(p2p.Network.findAndConnect[Effect], 0)
    } yield ()
}
