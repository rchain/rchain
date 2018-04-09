package coop.rchain.node

import coop.rchain.p2p, p2p.NetworkAddress, p2p.Network.KeysStore
import coop.rchain.p2p.effects._
import coop.rchain.comm._, CommError._
import com.typesafe.scalalogging.Logger
import coop.rchain.node.repl._
import coop.rchain.rholang.interpreter.Runtime
import java.util.UUID
import java.io.File

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval.Task

import scala.concurrent.{ExecutionContext, Future}

class NodeRuntime(conf: Conf) {

  import ApplicativeError_._

  /** Configuration */
  private val host           = conf.fetchHost()
  private val name           = conf.name.toOption.fold(UUID.randomUUID.toString.replaceAll("-", ""))(id)
  private val address        = s"rnode://$name@$host:${conf.port()}"
  private val src            = p2p.NetworkAddress.parse(address).right.get
  private val remoteKeysPath = System.getProperty("user.home") + File.separator + s".${name}-rnode-remote.keys"

  /** Run services */
  /** TODO all services should be defined in terms of `nodeProgram` */
  val metricsServer = MetricsServer()

  val http = HttpServer(conf.httpPort())
  http.start

  val runtime = Runtime.create()

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
  implicit val encryptionEffect: Encryption[Task]        = effects.encryption(name)
  implicit val logEffect: Log[Task]                      = effects.log
  implicit val timeEffect: Time[Task]                    = effects.time
  implicit val metricsEffect: Metrics[Task]              = effects.metrics
  implicit val inMemoryPeerKeysEffect: KeysStore[Task]   = effects.remoteKeysKvs(remoteKeysPath)
  implicit val communicatonEffect: Communication[Effect] = effects.communication[Effect](net)

  def connectToBootstrap: Effect[Unit] =
    for {
      bootstrapAddrStr <- conf.bootstrap.toOption
                           .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                           .toEffect
      bootstrapAddr <- p2p.NetworkAddress.parse(bootstrapAddrStr).toEffect
      _             <- Log[Effect].info(s"Bootstrapping from $bootstrapAddr.")
      _             <- p2p.Network.connect[Effect](bootstrapAddr)
      _             <- Log[Effect].info(s"Connected $bootstrapAddr.")
    } yield ()

  def addShutdownHook: Task[Unit] = Task.delay {
    sys.addShutdownHook {
      metricsServer.stop
      http.stop
      grpc.stop()
      net.broadcast(
        DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
      println("Goodbye.")
    }
  }

  def findAndConnect: Int => Effect[Int] = {

    val err: ApplicativeError_[Effect, CommError] = ApplicativeError_[Effect, CommError]

    (lastCount: Int) =>
      (for {
        _     <- IOUtil.sleep[Effect](5000L)
        peers <- Communication[Effect].findMorePeers(10)
        _ <- peers.toList.traverse(p => err.attempt(p2p.Network.connect[Effect](p))).map {
              attempts =>
                attempts.filter {
                  case Left(_) => false
                  case _       => true
                }
            }
        thisCount <- Communication[Effect].countPeers
        _ <- if (thisCount != lastCount) Log[Effect].info(s"Peers: $thisCount.")
            else ().pure[Effect]
      } yield thisCount)
  }

  val nodeProgram: Effect[Unit] = for {
    _ <- Task.fork(MonadOps.forever(net.receiver[Effect].value.void)).start.toEffect
    _ <- addShutdownHook.toEffect
    _ <- Log[Effect].info(s"Listening for traffic on $address.")
    _ <- if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
        else connectToBootstrap
    _ <- MonadOps.forever(findAndConnect, 0)
  } yield ()

}
