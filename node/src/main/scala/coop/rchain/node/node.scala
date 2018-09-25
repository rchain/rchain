package coop.rchain.node

import java.util.concurrent.TimeUnit

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import coop.rchain.blockstorage.{BlockStore, LMDBBlockStore}
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.util.comm.CasperPacketHandler
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{LastApprovedBlock, MultiParentCasperRef, SafetyOracle}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics
import coop.rchain.node.api._
import coop.rchain.node.configuration.Configuration
import coop.rchain.node.diagnostics._
import coop.rchain.shared.StoreType
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared._
import kamon._
import io.grpc.Server
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.server.{Server => Http4sServer}
import org.http4s.server.blaze._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class NodeRuntime(conf: Configuration, host: String)(implicit scheduler: Scheduler) {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  implicit def eiterTrpConfAsk(implicit ev: RPConfAsk[Task]): RPConfAsk[Effect] =
    new EitherTApplicativeAsk[Task, RPConf, CommError]

  private val dataDirFile = conf.server.dataDir.toFile

  if (!dataDirFile.exists()) {
    if (!dataDirFile.mkdir()) {
      println(
        s"The data dir must be a directory and have read and write permissions:\n${dataDirFile.getAbsolutePath}"
      )
      System.exit(-1)
    }
  }

  // Check if data_dir has read/write access
  if (!dataDirFile.isDirectory || !dataDirFile.canRead || !dataDirFile.canWrite) {
    println(
      s"The data dir must be a directory and have read and write permissions:\n${dataDirFile.getAbsolutePath}"
    )
    System.exit(-1)
  }

  println(s"Using data_dir: ${dataDirFile.getAbsolutePath}")

  // Generate certificate if not provided as option or in the data dir
  if (!conf.tls.customCertificateLocation
      && !conf.tls.certificate.toFile.exists()) {
    println(s"No certificate found at path ${conf.tls.certificate}")
    println("Generating a X.509 certificate for the node")

    import coop.rchain.shared.Resources._
    // If there is a private key, use it for the certificate
    if (conf.tls.key.toFile.exists()) {
      println(s"Using secret key ${conf.tls.key}")
      Try(CertificateHelper.readKeyPair(conf.tls.key.toFile)) match {
        case Success(keyPair) =>
          withResource(new java.io.PrintWriter(conf.tls.certificate.toFile)) {
            _.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
          }
        case Failure(e) =>
          println(s"Invalid secret key: ${e.getMessage}")
      }
    } else {
      println("Generating a PEM secret key for the node")
      val keyPair = CertificateHelper.generateKeyPair(conf.tls.secureRandomNonBlocking)
      withResource(new java.io.PrintWriter(conf.tls.certificate.toFile)) { pw =>
        pw.write(CertificatePrinter.print(CertificateHelper.generate(keyPair)))
      }
      withResource(new java.io.PrintWriter(conf.tls.key.toFile)) { pw =>
        pw.write(CertificatePrinter.printPrivateKey(keyPair.getPrivate))
      }
    }
  }

  if (!conf.tls.certificate.toFile.exists()) {
    println(s"Certificate file ${conf.tls.certificate} not found")
    System.exit(-1)
  }

  if (!conf.tls.key.toFile.exists()) {
    println(s"Secret key file ${conf.tls.certificate} not found")
    System.exit(-1)
  }

  private val name: String = {
    val publicKey = Try(CertificateHelper.fromFile(conf.tls.certificate.toFile)) match {
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
  private val port              = conf.server.port
  private val kademliaPort      = conf.server.kademliaPort
  private val address           = s"rnode://$name@$host?protocol=$port&discovery=$kademliaPort"
  private val storagePath       = conf.server.dataDir.resolve("rspace")
  private val casperStoragePath = storagePath.resolve("casper")
  private val storageSize       = conf.server.mapSize
  private val storeType         = conf.server.storeType
  private val defaultTimeout    = FiniteDuration(conf.server.defaultTimeout.toLong, MILLISECONDS) // TODO remove

  /** Final Effect + helper methods */
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  implicit class EitherEffectOps[A](e: Either[CommError, A]) {
    def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
  }
  implicit class TaskEffectOps[A](t: Task[A]) {
    def toEffect: Effect[A] = t.liftM[CommErrT]
  }

  case class Servers(
      grpcServerExternal: Server,
      grpcServerInternal: Server,
      httpServer: Http4sServer[IO]
  )

  def acquireServers(runtime: Runtime)(
      implicit
      log: Log[Task],
      nodeDiscovery: NodeDiscovery[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      multiParentCasperRef: MultiParentCasperRef[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task],
      connectionsCell: ConnectionsCell[Task]
  ): Effect[Servers] =
    for {
      grpcServerExternal <- GrpcServer
                             .acquireExternalServer[Effect](
                               conf.grpcServer.portExternal,
                               conf.server.maxMessageSize
                             )
      grpcServerInternal <- GrpcServer
                             .acquireInternalServer(
                               conf.grpcServer.portInternal,
                               conf.server.maxMessageSize,
                               runtime
                             )
                             .toEffect
      prometheusReporter = new NewPrometheusReporter()

      httpServer <- LiftIO[Task].liftIO {
                     val prometheusService     = NewPrometheusReporter.service(prometheusReporter)
                     implicit val contextShift = IO.contextShift(scheduler)
                     BlazeBuilder[IO]
                       .bindHttp(conf.server.httpPort, "0.0.0.0")
                       .mountService(prometheusService, "/metrics")
                       .mountService(VersionInfo.service, "/version")
                       .start
                   }.toEffect
      _ <- Task.delay {
            Kamon.addReporter(prometheusReporter)
            Kamon.addReporter(new JmxReporter())
          }.toEffect
    } yield Servers(grpcServerExternal, grpcServerInternal, httpServer)

  def startServers(servers: Servers)(
      implicit
      log: Log[Task]
  ): Effect[Unit] =
    GrpcServer.start(servers.grpcServerExternal, servers.grpcServerInternal).toEffect

  def clearResources(servers: Servers, runtime: Runtime, casperRuntime: Runtime)(
      implicit
      transport: TransportLayer[Task],
      log: Log[Task],
      blockStore: BlockStore[Effect],
      rpConfAsk: RPConfAsk[Task]
  ): Unit =
    (for {
      _   <- log.info("Shutting down gRPC servers...")
      _   <- Task.delay(servers.grpcServerExternal.shutdown())
      _   <- Task.delay(servers.grpcServerInternal.shutdown())
      _   <- log.info("Shutting down transport layer, broadcasting DISCONNECT")
      loc <- rpConfAsk.reader(_.local)
      msg = ProtocolHelper.disconnect(loc)
      _   <- transport.shutdown(msg)
      _   <- log.info("Shutting down HTTP server....")
      _   <- Task.delay(Kamon.stopAllReporters())
      _   <- LiftIO[Task].liftIO(servers.httpServer.shutdown)
      _   <- log.info("Shutting down interpreter runtime ...")
      _   <- Task.delay(runtime.close)
      _   <- log.info("Shutting down Casper runtime ...")
      _   <- Task.delay(casperRuntime.close)
      _   <- log.info("Bringing BlockStore down ...")
      _   <- blockStore.close().value
      _   <- log.info("Goodbye.")
    } yield ()).unsafeRunSync

  def startReportJvmMetrics(
      implicit metrics: Metrics[Task],
      jvmMetrics: JvmMetrics[Task]
  ): Task[Unit] =
    Task.delay {
      import scala.concurrent.duration._
      scheduler.scheduleAtFixedRate(3.seconds, 3.second)(JvmMetrics.report[Task].unsafeRunSync)
    }

  def addShutdownHook(servers: Servers, runtime: Runtime, casperRuntime: Runtime)(
      implicit transport: TransportLayer[Task],
      log: Log[Task],
      blockStore: BlockStore[Effect],
      rpConfAsk: RPConfAsk[Task]
  ): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(servers, runtime, casperRuntime)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  private def nodeProgram(runtime: Runtime, casperRuntime: Runtime)(
      implicit
      log: Log[Task],
      time: Time[Task],
      timerTask: Timer[Task],
      rpConfAsk: RPConfAsk[Task],
      metrics: Metrics[Task],
      transport: TransportLayer[Task],
      nodeDiscovery: NodeDiscovery[Task],
      rpConnectons: ConnectionsCell[Task],
      blockStore: BlockStore[Effect],
      oracle: SafetyOracle[Effect],
      packetHandler: PacketHandler[Effect],
      casperConstructor: MultiParentCasperRef[Effect],
      nodeCoreMetrics: NodeMetrics[Task],
      jvmMetrics: JvmMetrics[Task]
  ): Effect[Unit] = {

    val info: Effect[Unit] = for {
      _ <- Log[Effect].info(VersionInfo.get)
      _ <- if (conf.server.standalone) Log[Effect].info(s"Starting stand-alone node.")
          else Log[Effect].info(s"Starting node that will bootstrap from ${conf.server.bootstrap}")
    } yield ()

    val loop: Effect[Unit] = for {
      _ <- Connect.clearConnections[Effect]
      _ <- Connect.findAndConnect[Effect](Connect.connect[Effect])
      _ <- timerTask.sleep(5.seconds).toEffect
    } yield ()

    for {
      _       <- info
      servers <- acquireServers(runtime)
      _       <- addShutdownHook(servers, runtime, casperRuntime).toEffect
      _       <- startServers(servers)
      _       <- startReportJvmMetrics.toEffect
      _       <- TransportLayer[Effect].receive(pm => HandleMessages.handle[Effect](pm, defaultTimeout))
      _       <- NodeDiscovery[Task].discover.fork.toEffect
      _       <- Log[Effect].info(s"Listening for traffic on $address.")
      _       <- loop.forever
    } yield ()
  }

  /**
    * Handles unrecoverable errors in program. Those are errors that should not happen in properly
    * configured enviornment and they mean immediate termination of the program
    */
  private def handleUnrecoverableErrors(prog: Effect[Unit])(implicit log: Log[Task]): Effect[Unit] =
    EitherT[Task, CommError, Unit](
      prog.value
        .onErrorHandleWith {
          case th =>
            log.error("Caught unhandable error. Exiting. Stacktrace below.") *> Task.delay {
              th.printStackTrace();
            }
        } *> exit0.as(Right(()))
    )

  private def timerEff(implicit timerTask: Timer[Task]): Timer[Effect] =
    Timer.deriveEitherT(Functor[Task], timerTask)

  private val syncEffect = SyncInstances.syncEffect[CommError](commError => {
    new Exception(s"CommError: $commError")
  }, e => { UnknownCommError(e.getMessage) })

  /**
    * Main node entry. It will:
    * 1. set up configurations
    * 2. create instances of typeclasses
    * 3. run the node program.
    */
  val main: Effect[Unit] = for {
    // 1. set up configurations
    local          <- EitherT.fromEither[Task](PeerNode.fromAddress(address))
    defaultTimeout = conf.server.defaultTimeout.millis
    rpClearConnConf = ClearConnetionsConf(
      conf.server.maxNumOfConnections,
      numOfConnectionsPinged = 10
    ) // TODO read from conf
    // 2. create instances of typeclasses
    rpConfAsk            = effects.rpConfAsk(RPConf(local, defaultTimeout, rpClearConnConf))
    tcpConnections       <- effects.tcpConnections.toEffect
    rpConnections        <- effects.rpConnections.toEffect
    log                  = effects.log
    time                 = effects.time
    timerTask            = Task.timer
    metrics              = diagnostics.metrics[Task]
    multiParentCasperRef <- MultiParentCasperRef.of[Effect]
    lab                  <- LastApprovedBlock.of[Task].toEffect
    labEff               = LastApprovedBlock.eitherTLastApprovedBlock[CommError, Task](Monad[Task], lab)
    transport = effects.tcpTransportLayer(
      host,
      port,
      conf.tls.certificate,
      conf.tls.key,
      conf.server.maxMessageSize
    )(scheduler, tcpConnections, log)
    kademliaRPC = effects.kademliaRPC(local, kademliaPort, defaultTimeout)(scheduler, metrics, log)
    initPeer    = if (conf.server.standalone) None else Some(conf.server.bootstrap)
    nodeDiscovery <- effects
                      .nodeDiscovery(local, defaultTimeout)(initPeer)(
                        log,
                        timerTask,
                        metrics,
                        kademliaRPC
                      )
                      .toEffect
    blockStore = LMDBBlockStore.create[Effect](conf.blockstorage)(
      syncEffect,
      Metrics.eitherT(Monad[Task], metrics)
    )

    _              <- blockStore.clear() // TODO: Replace with a proper casper init when it's available
    oracle         = SafetyOracle.turanOracle[Effect](Monad[Effect], blockStore)
    runtime        = Runtime.create(storagePath, storageSize, storeType)
    casperRuntime  = Runtime.create(casperStoragePath, storageSize, storeType)
    runtimeManager = RuntimeManager.fromRuntime(casperRuntime)
    casperPacketHandler <- CasperPacketHandler
                            .of[Effect](conf.casper, defaultTimeout, runtimeManager, _.value)(
                              labEff,
                              Metrics.eitherT(Monad[Task], metrics),
                              timerEff(timerTask),
                              blockStore,
                              Cell.eitherTCell(Monad[Task], rpConnections),
                              NodeDiscovery.eitherTNodeDiscovery(Monad[Task], nodeDiscovery),
                              TransportLayer.eitherTTransportLayer(Monad[Task], log, transport),
                              ErrorHandler[Effect],
                              eiterTrpConfAsk(rpConfAsk),
                              oracle,
                              Capture[Effect],
                              Sync[Effect],
                              Time.eitherTTime(Monad[Task], time),
                              Log.eitherTLog(Monad[Task], log),
                              multiParentCasperRef,
                              scheduler
                            )
    packetHandler = PacketHandler.pf[Effect](casperPacketHandler.handle)(
      Applicative[Effect],
      Log.eitherTLog(Monad[Task], log),
      ErrorHandler[Effect]
    )
    nodeCoreMetrics = diagnostics.nodeCoreMetrics[Task]
    jvmMetrics      = diagnostics.jvmMetrics[Task]
    // 3. run the node program.
    program = nodeProgram(runtime, casperRuntime)(
      log,
      time,
      timerTask,
      rpConfAsk,
      metrics,
      transport,
      nodeDiscovery,
      rpConnections,
      blockStore,
      oracle,
      packetHandler,
      multiParentCasperRef,
      nodeCoreMetrics,
      jvmMetrics
    )
    _ <- handleUnrecoverableErrors(program)(log)
  } yield ()

}
