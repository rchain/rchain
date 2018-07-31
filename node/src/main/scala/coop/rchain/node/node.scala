package coop.rchain.node

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import io.grpc.Server
import cats._, cats.data._, cats.implicits._, cats.mtl._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import coop.rchain.casper.{MultiParentCasperConstructor, SafetyOracle}
import coop.rchain.casper.util.comm.CommUtil.{casperPacketHandler, requestApprovedBlock}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm._
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics
import coop.rchain.node.diagnostics._
import coop.rchain.p2p
import coop.rchain.p2p.effects._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Resources._
import monix.eval.Task
import monix.execution.Scheduler
import diagnostics.MetricsServer
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import ThrowableOps._
import cats.effect.Sync
import coop.rchain.blockstorage.{BlockStore, LMDBBlockStore}
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

  case class Resources(grpcServer: Server, metricsServer: MetricsServer, httpServer: HttpServer)

  def acquireResources(runtime: Runtime)(
      implicit
      log: Log[Task],
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      casperConstructor: MultiParentCasperConstructor[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task]
  ): Effect[Resources] =
    for {
      grpcServer    <- GrpcServer.acquireServer[Effect](conf.grpcPort(), runtime)
      metricsServer <- MetricsServer.create[Effect](conf.run.metricsPort())
      httpServer    <- HttpServer(conf.run.httpPort()).pure[Effect]
    } yield Resources(grpcServer, metricsServer, httpServer)

  def startResources(resources: Resources)(
      implicit
      log: Log[Task],
  ): Effect[Unit] =
    for {
      _ <- resources.httpServer.start.toEffect
      _ <- resources.metricsServer.start.toEffect
      _ <- GrpcServer.start[Effect](resources.grpcServer)
    } yield ()

  def clearResources(resources: Resources, runtime: Runtime, casperRuntime: Runtime)(
      implicit
      time: Time[Task],
      transport: TransportLayer[Task],
      log: Log[Task],
      blockStore: BlockStore[Effect]): Unit =
    (for {
      _   <- log.info("Shutting down gRPC server...")
      _   <- Task.delay(resources.grpcServer.shutdown())
      _   <- log.info("Shutting down transport layer, broadcasting DISCONNECT")
      loc <- transport.local
      ts  <- time.currentMillis
      msg = ProtocolHelper.disconnect(loc)
      _   <- transport.shutdown(msg)
      _   <- log.info("Shutting down metrics server...")
      _   <- Task.delay(resources.metricsServer.stop())
      _   <- log.info("Shutting down HTTP server....")
      _   <- Task.delay(resources.httpServer.stop())
      _   <- log.info("Shutting down interpreter runtime ...")
      _   <- Task.delay(runtime.close)
      _   <- log.info("Shutting down Casper runtime ...")
      _   <- Task.delay(casperRuntime.close)
      _   <- log.info("Bringing BlockStore down ...")
      _   <- blockStore.close().value
      _   <- log.info("Goodbye.")
    } yield ()).unsafeRunSync

  def startReportJvmMetrics(implicit metrics: Metrics[Task],
                            jvmMetrics: JvmMetrics[Task]): Task[Unit] =
    Task.delay {
      import scala.concurrent.duration._
      scheduler.scheduleAtFixedRate(3.seconds, 3.second)(JvmMetrics.report[Task].unsafeRunSync)
    }

  def addShutdownHook(resources: Resources, runtime: Runtime, casperRuntime: Runtime)(
      implicit transport: TransportLayer[Task],
      log: Log[Task],
      time: Time[Task],
      blockStore: BlockStore[Effect]): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(resources, runtime, casperRuntime)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  def handleCommunications(resources: Resources)(
      implicit
      log: Log[Task],
      time: Time[Task],
      metrics: Metrics[Task],
      transport: TransportLayer[Task],
      nodeDiscovery: NodeDiscovery[Task],
      packetHandler: PacketHandler[Effect]): Protocol => Effect[CommunicationResponse] = {

    (pm: Protocol) =>
      NodeDiscovery[Effect].handleCommunications(pm) >>= {
        case NotHandled(_) => Connect.dispatch[Effect](pm, defaultTimeout)
        case handled       => handled.pure[Effect]
      }
  }

  private def nodeProgram(runtime: Runtime, casperRuntime: Runtime)(
      implicit
      log: Log[Task],
      time: Time[Task],
      metrics: Metrics[Task],
      transport: TransportLayer[Task],
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      packetHandler: PacketHandler[Effect],
      casperConstructor: MultiParentCasperConstructor[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task]
  ): Effect[Unit] =
    for {
      _ <- Log[Effect].info(
            s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})")
      resources <- acquireResources(runtime)
      _         <- addShutdownHook(resources, runtime, casperRuntime).toEffect
      _         <- startResources(resources)
      _         <- startReportJvmMetrics.toEffect
      _         <- TransportLayer[Effect].receive(handleCommunications(resources))
      _         <- Log[Effect].info(s"Listening for traffic on $address.")
      res <- ApplicativeError_[Effect, CommError].attempt(
              if (conf.run.standalone()) Log[Effect].info(s"Starting stand-alone node.")
              else
                conf.run.bootstrap.toOption
                  .fold[Either[CommError, PeerNode]](Left(BootstrapNotProvided))(Right(_))
                  .toEffect >>= (
                    addr =>
                      Connect.connectToBootstrap[Effect](addr,
                                                         maxNumOfAttempts = 5,
                                                         defaultTimeout = defaultTimeout)))
      _ <- if (res.isRight)
            MonadOps.forever((i: Int) =>
                               Connect
                                 .findAndConnect[Effect](defaultTimeout)
                                 .apply(i) <* requestApprovedBlock[Effect],
                             0)
          else ().pure[Effect]

      _ <- exit0.toEffect
    } yield ()

  /**
    * Handles unrecoverable errors in program. Those are errors that should not happen in properly
    * configured enviornment and they mean immediate termination of the program
    */
  private def handleUnrecoverableErrors(prog: Effect[Unit])(implicit log: Log[Task]): Effect[Unit] =
    EitherT[Task, CommError, Unit](
      prog.value
        .onErrorHandleWith {
          case th if th.containsMessageWith("Error loading shared library libsodium.so") =>
            Log[Task]
              .error(
                "Libsodium is NOT installed on your system. Please install libsodium (https://github.com/jedisct1/libsodium) and try again.")
          case th =>
            th.getStackTrace.toList.traverse(ste => Log[Task].error(ste.toString))
        } *> exit0.as(Right(())))

  private def generateCasperConstructor(runtimeManager: RuntimeManager)(
      implicit
      log: Log[Task],
      time: Time[Task],
      transport: TransportLayer[Task],
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect]): Effect[MultiParentCasperConstructor[Effect]] =
    MultiParentCasperConstructor.fromConfig[Effect, Effect](conf.casperConf, runtimeManager)

  private def generateCasperPacketHandler(implicit
                                          log: Log[Task],
                                          time: Time[Task],
                                          transport: TransportLayer[Task],
                                          nodeDiscovery: NodeDiscovery[Task],
                                          blockStore: BlockStore[Effect],
                                          casperConstructor: MultiParentCasperConstructor[Effect])
    : PeerNode => PartialFunction[Packet, Effect[Option[Packet]]] =
    casperPacketHandler[Effect](_)

  /**
    * Main node entry. Will Create instances of typeclasses and run the node program.
    */
  val main: Effect[Unit] = for {

    /** create typeclass instances */
    tcpConnections <- effects.tcpConnections.toEffect
    log            = effects.log
    time           = effects.time
    sync           = SyncInstances.syncEffect
    metrics        = diagnostics.metrics[Task]
    transport = effects.tcpTransportLayer(host, port, certificateFile, keyFile)(src)(scheduler,
                                                                                     tcpConnections,
                                                                                     log)
    kademliaRPC = effects.kademliaRPC(src, defaultTimeout)(metrics, transport)
    nodeDiscovery = effects.nodeDiscovery(src, defaultTimeout)(log,
                                                               time,
                                                               metrics,
                                                               transport,
                                                               kademliaRPC)
    blockStore = LMDBBlockStore.create[Effect](conf.casperBlockStoreConf)(
      sync,
      Metrics.eitherT(Monad[Task], metrics))
    _              <- blockStore.clear() // FIX-ME replace with a proper casper init when it's available
    oracle         = SafetyOracle.turanOracle[Effect](Applicative[Effect], blockStore)
    runtime        = Runtime.create(storagePath, storageSize)
    casperRuntime  = Runtime.create(casperStoragePath, storageSize)
    runtimeManager = RuntimeManager.fromRuntime(casperRuntime)
    casperConstructor <- generateCasperConstructor(runtimeManager)(log,
                                                                   time,
                                                                   transport,
                                                                   nodeDiscovery,
                                                                   blockStore,
                                                                   oracle)
    cph = generateCasperPacketHandler(log,
                                      time,
                                      transport,
                                      nodeDiscovery,
                                      blockStore,
                                      casperConstructor)
    packetHandler = PacketHandler.pf[Effect](cph)(Applicative[Effect],
                                                  Log.eitherTLog(Monad[Task], log),
                                                  ErrorHandler[Effect])
    nodeCoreMetrics = diagnostics.nodeCoreMetrics[Task]
    jvmMetrics      = diagnostics.jvmMetrics[Task]

    /** run the node program */
    program = nodeProgram(runtime, casperRuntime)(log,
                                                  time,
                                                  metrics,
                                                  transport,
                                                  nodeDiscovery,
                                                  blockStore,
                                                  oracle,
                                                  packetHandler,
                                                  casperConstructor,
                                                  nodeCoreMetrics,
                                                  jvmMetrics)
    _ <- handleUnrecoverableErrors(program)(log)
  } yield ()

}
