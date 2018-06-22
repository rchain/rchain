package coop.rchain.node

import java.io.{File, PrintWriter}

import io.grpc.Server
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._
import ski._
import TaskContrib._
import coop.rchain.casper.{MultiParentCasper, SafetyOracle}
import coop.rchain.casper.genesis.Genesis.fromBondsFile
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.comm._
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics
import coop.rchain.node.diagnostics._
import coop.rchain.p2p
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Resources._
import monix.eval.Task
import monix.execution.Scheduler
import diagnostics.MetricsServer
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._, ThrowableOps._
import coop.rchain.node.api._
import coop.rchain.comm.connect.Connect

import scala.io.Source
import scala.util.{Failure, Success, Try}

class NodeRuntime(conf: Conf)(implicit scheduler: Scheduler) {

  // Generate certificate if not provided as option or in the data dir
  if (conf.run.certificate.toOption.isEmpty
      && !conf.run.certificatePath.toFile.exists()) {
    println(s"No certificate found at path ${conf.run.certificatePath}")
    println("Generating a X.509 certificate for the node")

    import coop.rchain.shared.Resources._
    // If there is a private key, use it for the certificate
    if (conf.run.keyPath.toFile.exists()) {
      println(s"Using secret key ${conf.run.keyPath}")
      Try(CertificateHelper.readKeyPair(conf.run.keyPath.toFile)) match {
        case Success(keyPair) =>
          withResource(new java.io.PrintWriter(conf.run.certificatePath.toFile)) {
            _.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
          }
        case Failure(e) =>
          println(s"Invalid secret key: ${e.getMessage}")
      }
    } else {
      println("Generating a PEM secret key for the node")
      val keyPair = CertificateHelper.generateKeyPair()
      withResource(new java.io.PrintWriter(conf.run.certificatePath.toFile)) { pw =>
        pw.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
      }
      withResource(new java.io.PrintWriter(conf.run.keyPath.toFile)) { pw =>
        pw.write(CertificatePrinter.printPrivateKey(keyPair.getPrivate))
      }
    }
  }

  if (!conf.run.certificatePath.toFile.exists()) {
    println(s"Certificate file ${conf.run.certificatePath} not found")
    System.exit(-1)
  }

  if (!conf.run.keyPath.toFile.exists()) {
    println(s"Secret key file ${conf.run.keyPath} not found")
    System.exit(-1)
  }

  private val name: String = {
    val certPath = conf.run.certificate.toOption
      .getOrElse(java.nio.file.Paths.get(conf.run.data_dir().toString, "node.certificate.pem"))

    val publicKey = Try(CertificateHelper.fromFile(certPath.toFile)) match {
      case Success(c) => Some(c.getPublicKey)
      case Failure(e) =>
        println(s"Failed to read the X.509 certificate: ${e.getMessage}")
        System.exit(1)
        None
      case _ => None
    }

    val publicKeyHash = publicKey
      .flatMap(CertificateHelper.publicAddress)
      .map(Base16.encode)

    if (publicKeyHash.isEmpty) {
      println("Certificate must contain a secp256r1 EC Public Key")
      System.exit(1)
    }

    publicKeyHash.get
  }

  import ApplicativeError_._

  /** Configuration */
  private val host        = conf.run.localhost
  private val address     = s"rnode://$name@$host:${conf.run.port()}"
  private val src         = PeerNode.parse(address).right.get
  private val storagePath = conf.run.data_dir().resolve("rspace")
  private val storageSize = conf.run.map_size()

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
  implicit val logEffect: Log[Task]                     = effects.log
  implicit val timeEffect: Time[Task]                   = effects.time
  implicit val jvmMetricsEffect: JvmMetrics[Task]       = diagnostics.jvmMetrics
  implicit val metricsEffect: Metrics[Task]             = diagnostics.metrics
  implicit val nodeCoreMetricsEffect: NodeMetrics[Task] = diagnostics.nodeCoreMetrics
  implicit val transportLayerEffect: TransportLayer[Task] =
    effects.tcpTranposrtLayer[Task](conf)(src)
  implicit val pingEffect: Ping[Task]                   = effects.ping(src)
  implicit val nodeDiscoveryEffect: NodeDiscovery[Task] = new TLNodeDiscovery[Task](src)
  implicit val turanOracleEffect: SafetyOracle[Effect]  = SafetyOracle.turanOracle[Effect]
  implicit val casperEffect: MultiParentCasper[Effect] =
    MultiParentCasper.fromConfig[Effect](conf.casperConf)
  implicit val packetHandlerEffect: PacketHandler[Effect] = PacketHandler.pf[Effect](
    casperPacketHandler[Effect]
  )

  case class Resources(grpcServer: Server,
                       metricsServer: MetricsServer,
                       httpServer: HttpServer,
                       runtime: Runtime)

  def acquireResources: Effect[Resources] =
    for {
      runtime <- Runtime.create(storagePath, storageSize).pure[Effect]
      grpcServer <- {
        implicit val storeMetrics =
          diagnostics.storeMetrics[Effect](runtime.space.store, conf.run.data_dir().normalize)
        GrpcServer
          .acquireServer[Effect](conf.grpcPort(), runtime)
      }
      metricsServer <- MetricsServer.create[Effect](conf.run.metricsPort())
      httpServer    <- HttpServer(conf.run.httpPort()).pure[Effect]
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

    (for {
      peers <- nodeDiscoveryEffect.peers
      loc   <- transportLayerEffect.local
      ts    <- timeEffect.currentMillis
      msg   = DisconnectMessage(ProtocolMessage.disconnect(loc), ts)
      _     <- transportLayerEffect.broadcast(msg, peers)
    } yield ()).unsafeRunSync
    println("Shutting down metrics server...")
    resources.metricsServer.stop()
    println("Shutting down HTTP server....")
    resources.httpServer.stop()
    println("Shutting down interpreter runtime ...")
    resources.runtime.close()

    println("Goodbye.")
  }

  def startReportJvmMetrics: Task[Unit] =
    Task.delay {
      import scala.concurrent.duration._
      scheduler.scheduleAtFixedRate(3.seconds, 3.second)(JvmMetrics.report[Task].unsafeRunSync)
    }

  def startReportStoreMetrics(resources: Resources): Task[Unit] =
    Task.delay {
      import scala.concurrent.duration._
      implicit val storeMetrics: StoreMetrics[Task] =
        diagnostics.storeMetrics[Task](resources.runtime.space.store, conf.run.data_dir().normalize)
      scheduler.scheduleAtFixedRate(10.seconds, 10.second)(StoreMetrics.report[Task].unsafeRunSync)
    }

  def addShutdownHook(resources: Resources): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(resources)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  def handleCommunications: ProtocolMessage => Effect[CommunicationResponse] =
    pm =>
      NodeDiscovery[Effect].handleCommunications(pm) >>= {
        case NotHandled => Connect.dispatch[Effect](pm)
        case handled    => handled.pure[Effect]
    }

  private def unrecoverableNodeProgram: Effect[Unit] =
    for {
      _         <- Log[Effect].info(s"RChain Node ${BuildInfo.version}")
      resources <- acquireResources
      _         <- startResources(resources)
      _         <- addShutdownHook(resources).toEffect
      _         <- startReportJvmMetrics.toEffect
      _         <- startReportStoreMetrics(resources).toEffect
      _         <- TransportLayer[Effect].receive(handleCommunications)
      _         <- Log[Effect].info(s"Listening for traffic on $address.")
      res <- ApplicativeError_[Effect, CommError].attempt(
              if (conf.run.standalone()) Log[Effect].info(s"Starting stand-alone node.")
              else
                conf.run.bootstrap.toOption
                  .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                  .toEffect >>= (addr => Connect.connectToBootstrap[Effect](addr)))
      _ <- if (res.isRight) MonadOps.forever(Connect.findAndConnect[Effect], 0)
          else ().pure[Effect]
      _ <- casperEffect.close()
      _ <- exit0.toEffect
    } yield ()

  def nodeProgram: Effect[Unit] =
    EitherT[Task, CommError, Unit](unrecoverableNodeProgram.value.onErrorHandleWith {
      case th if th.containsMessageWith("Error loading shared library libsodium.so") =>
        Log[Task]
          .error(
            "Libsodium is NOT installed on your system. Please install libsodium (https://github.com/jedisct1/libsodium) and try again.")
      case th =>
        th.getStackTrace.toList.traverse(ste => Log[Task].error(ste.toString))
    } *> exit0.as(Right(())))
}
