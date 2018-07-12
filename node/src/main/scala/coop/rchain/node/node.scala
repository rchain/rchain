package coop.rchain.node

import java.io.{File, PrintWriter}

import io.grpc.Server
import cats._
import cats.data._
import cats.implicits._
import cats.mtl._
import coop.rchain.catscontrib._
import Catscontrib._
import ski._
import TaskContrib._
import coop.rchain.casper.{MultiParentCasperConstructor, SafetyOracle}
import coop.rchain.casper.util.comm.CommUtil.{casperPacketHandler, requestApprovedBlock}
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
import coop.rchain.comm.discovery.{Ping => NDPing, _}
import coop.rchain.shared._, ThrowableOps._
import coop.rchain.node.api._
import coop.rchain.comm.connect.Connect
import coop.rchain.comm.protocol.routing._
import coop.rchain.crypto.codec.Base16
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import coop.rchain.comm.transport.TcpTransportLayer.Connections

class NodeRuntime(conf: Conf)(implicit scheduler: Scheduler) {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val dataDirFile = conf.run.data_dir().toFile

  if (!dataDirFile.exists()) {
    if (!dataDirFile.mkdir()) {
      println(
        s"The data dir must be a directory and have read and write permissions:\n${conf.run.data_dir()}")
      System.exit(-1)
    }
  }

  // Check if data_dir has read/write access
  if (!dataDirFile.isDirectory || !dataDirFile.canRead || !dataDirFile.canWrite) {
    println(
      s"The data dir must be a directory and have read and write permissions:\n${conf.run.data_dir()}")
    System.exit(-1)
  }

  println(s"Using data_dir: ${conf.run.data_dir()}")

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

  /**
    TODO FIX-ME This should not be here. Please fix this when working on rnode-0.5.x
    This needs to be moved to node program! Part of execution. Effectful!
    */
  private val externalAddress = if (conf.run.noUpnp()) {
    None
  } else {
    UPnP.assurePortForwarding(Seq(conf.run.port()))
  }

  import ApplicativeError_._

  /** Configuration */
  private val host              = conf.run.fetchHost(externalAddress)
  private val port              = conf.run.port()
  private val certificateFile   = conf.run.certificatePath.toFile
  private val keyFile           = conf.run.keyPath.toFile
  private val address           = s"rnode://$name@$host:$port"
  private val src               = PeerNode.parse(address).right.get
  private val storagePath       = conf.run.data_dir().resolve("rspace")
  private val casperStoragePath = storagePath.resolve("casper")
  private val storageSize       = conf.run.map_size()
  private val defaultTimeout    = FiniteDuration(conf.run.defaultTimeout().toLong, MILLISECONDS)

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
  implicit val logEffect: Log[Task]                               = effects.log
  implicit val timeEffect: Time[Task]                             = effects.time
  implicit val jvmMetricsEffect: JvmMetrics[Task]                 = diagnostics.jvmMetrics
  implicit val metricsEffect: Metrics[Task]                       = diagnostics.metrics
  implicit val nodeCoreMetricsEffect: NodeMetrics[Task]           = diagnostics.nodeCoreMetrics
  implicit val connectionsState: MonadState[Task, TransportState] = effects.connectionsState[Task]
  implicit val transportLayerEffect: TransportLayer[Task] =
    effects.tcpTranposrtLayer(host, port, certificateFile, keyFile)(src)
  implicit val pingEffect: NDPing[Task] = effects.ping(src, defaultTimeout)
  implicit val nodeDiscoveryEffect: NodeDiscovery[Task] =
    new TLNodeDiscovery[Task](src, defaultTimeout)
  implicit val turanOracleEffect: SafetyOracle[Effect] = SafetyOracle.turanOracle[Effect]

  case class Resources(grpcServer: Server,
                       metricsServer: MetricsServer,
                       httpServer: HttpServer,
                       runtime: Runtime,
                       casperRuntime: Runtime,
                       casperConstructor: MultiParentCasperConstructor[Effect],
                       packetHandler: PacketHandler[Effect])

  def acquireResources: Effect[Resources] =
    for {
      runtime       <- Runtime.create(storagePath, storageSize).pure[Effect]
      casperRuntime <- Runtime.create(casperStoragePath, storageSize).pure[Effect]
      casperConstructor <- MultiParentCasperConstructor
                            .fromConfig[Effect, Effect](conf.casperConf, casperRuntime)
      grpcServer <- {
        implicit val casperEvidence: MultiParentCasperConstructor[Effect] = casperConstructor
        implicit val storeMetrics =
          diagnostics.storeMetrics[Effect](casperRuntime.space.store,
                                           casperRuntime.replaySpace.store,
                                           conf.run.data_dir().normalize)
        GrpcServer
          .acquireServer[Effect](conf.grpcPort(), runtime)
      }
      metricsServer <- MetricsServer.create[Effect](conf.run.metricsPort())
      httpServer    <- HttpServer(conf.run.httpPort()).pure[Effect]
    } yield {
      implicit val casperEvidence: MultiParentCasperConstructor[Effect] = casperConstructor
      val packetHandlerEffect = PacketHandler.pf[Effect](
        casperPacketHandler[Effect]
      )
      Resources(grpcServer,
                metricsServer,
                httpServer,
                runtime,
                casperRuntime,
                casperConstructor,
                packetHandlerEffect)
    }

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
      loc <- transportLayerEffect.local
      ts  <- timeEffect.currentMillis
      msg = ProtocolHelper.disconnect(loc)
      _   <- transportLayerEffect.shutdown(msg)
    } yield ()).unsafeRunSync
    println("Shutting down metrics server...")
    resources.metricsServer.stop()
    println("Shutting down HTTP server....")
    resources.httpServer.stop()
    println("Shutting down interpreter runtime ...")
    resources.runtime.close()
    println("Shutting down Casper runtime ...")
    resources.casperRuntime.close()

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
        diagnostics.storeMetrics[Task](resources.casperRuntime.space.store,
                                       resources.casperRuntime.replaySpace.store,
                                       conf.run.data_dir().normalize)
      scheduler.scheduleAtFixedRate(10.seconds, 10.second)(StoreMetrics.report[Task].unsafeRunSync)
    }

  def addShutdownHook(resources: Resources): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(resources)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  def handleCommunications(resources: Resources): Protocol => Effect[CommunicationResponse] = {
    implicit val packetHandlerEffect: PacketHandler[Effect] = resources.packetHandler

    (pm: Protocol) =>
      NodeDiscovery[Effect].handleCommunications(pm) >>= {
        case NotHandled(_) => Connect.dispatch[Effect](pm)
        case handled       => handled.pure[Effect]
      }
  }

  private def unrecoverableNodeProgram: Effect[Unit] =
    for {
      _ <- Log[Effect].info(
            s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})")
      resources <- acquireResources
      _         <- startResources(resources)
      _         <- addShutdownHook(resources).toEffect
      _         <- startReportJvmMetrics.toEffect
      _         <- startReportStoreMetrics(resources).toEffect
      _         <- TransportLayer[Effect].receive(handleCommunications(resources))
      _         <- Log[Effect].info(s"Listening for traffic on $address.")
      res <- ApplicativeError_[Effect, CommError].attempt(
              if (conf.run.standalone()) Log[Effect].info(s"Starting stand-alone node.")
              else
                conf.run.bootstrap.toOption
                  .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                  .toEffect >>= (
                    addr =>
                      Connect.connectToBootstrap[Effect](addr,
                                                         maxNumOfAttempts = 5,
                                                         defaultTimeout = defaultTimeout)))
      _ <- {
        implicit val casperEvidence      = resources.casperConstructor
        implicit val packetHandlerEffect = resources.packetHandler
        if (res.isRight)
          MonadOps.forever((i: Int) =>
                             Connect
                               .findAndConnect[Effect](defaultTimeout)
                               .apply(i) <* requestApprovedBlock[Effect],
                           0)
        else ().pure[Effect]
      }
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
