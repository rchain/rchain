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
import coop.rchain.casper.util.ProtoUtil.genesisBlock
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.comm._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.node.diagnostics._
import coop.rchain.p2p
import coop.rchain.p2p.Network.KeysStore
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Resources._
import monix.eval.Task
import monix.execution.Scheduler
import diagnostics.MetricsServer
import coop.rchain.node.effects.TLNodeDiscovery

import scala.io.Source
import scala.util.{Failure, Success, Try}

class NodeRuntime(conf: Conf)(implicit scheduler: Scheduler) {

  // Generate certificate if not provided as option or in the data dir
  if (conf.certificate.toOption.isEmpty
      && conf.key.toOption.isEmpty
      && !conf.certificatePath.toFile.exists()) {
    println(s"No certificate found at path ${conf.certificatePath}")
    println("Generating a X.509 certificate and secret key for the node")

    import coop.rchain.shared.Resources._
    val keyPair = CertificateHelper.generateKeyPair()
    val cert    = CertificateHelper.generate(keyPair)
    withResource(new java.io.PrintWriter(conf.certificatePath.toFile)) { pw =>
      pw.write(CertificatePrinter.print(cert))
    }
    withResource(new java.io.PrintWriter(conf.keyPath.toFile)) { pw =>
      pw.write(CertificatePrinter.printPrivateKey(keyPair.getPrivate))
    }
  }

  if (!conf.certificatePath.toFile.exists()) {
    println(s"Certificate file ${conf.certificatePath} not found")
    System.exit(-1)
  }

  if (!conf.keyPath.toFile.exists()) {
    println(s"Secret key file ${conf.keyPath} not found")
    System.exit(-1)
  }

  private val name: String = {
    val certPath = conf.certificate.toOption
      .getOrElse(java.nio.file.Paths.get(conf.data_dir().toString, "node.certificate.pem"))

    val publicKey = Try(CertificateHelper.fromFile(certPath.toFile)) match {
      case Success(c) => Some(c.getPublicKey)
      case Failure(e) =>
        println(s"Failed to read the X.509 certificate: ${e.getMessage}")
        System.exit(1)
        None
      case _ => None
    }

    publicKey
      .flatMap(CertificateHelper.publicAddress)
      .getOrElse {
        println("Certificate must contain a secp256r1 EC Public Key")
        System.exit(1)
        Array[Byte]()
      }
      .map("%02x".format(_))
      .mkString
  }

  implicit class ThrowableOps(th: Throwable) {
    def containsMessageWith(str: String): Boolean =
      if (th.getCause == null) th.getMessage.contains(str)
      else th.getMessage.contains(str) || th.getCause.containsMessageWith(str)
  }

  import ApplicativeError_._

  /** Configuration */
  private val host           = conf.localhost
  private val address        = s"rnode://$name@$host:${conf.port()}"
  private val src            = p2p.NetworkAddress.parse(address).right.get
  private val remoteKeysPath = conf.data_dir().resolve("keys").resolve(s"$name-rnode-remote.keys")
  private val keysPath       = conf.data_dir().resolve("keys").resolve(s"$name-rnode.keys")
  private val storagePath    = conf.data_dir().resolve("rspace")
  private val storageSize    = conf.map_size()

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
  implicit val encryptionEffect: Encryption[Task]       = effects.encryption(keysPath)
  implicit val logEffect: Log[Task]                     = effects.log
  implicit val timeEffect: Time[Task]                   = effects.time
  implicit val jvmMetricsEffect: JvmMetrics[Task]       = diagnostics.jvmMetrics
  implicit val metricsEffect: Metrics[Task]             = diagnostics.metrics
  implicit val nodeCoreMetricsEffect: NodeMetrics[Task] = diagnostics.nodeCoreMetrics
  implicit val inMemoryPeerKeysEffect: KeysStore[Task]  = effects.remoteKeysKvs(remoteKeysPath)
  implicit val transportLayerEffect: TransportLayer[Task] =
    effects.tcpTranposrtLayer[Task](conf)(src)
  implicit val pingEffect: Ping[Task]                   = effects.ping(src)
  implicit val nodeDiscoveryEffect: NodeDiscovery[Task] = new TLNodeDiscovery[Task](src)

  val bondsFile: Option[File] =
    conf.bondsFile.toOption
      .flatMap(path => {
        val f = new File(path)
        if (f.exists) Some(f)
        else {
          Log[Task].warn(
            s"Specified bonds file $path does not exist. Falling back on generating random validators."
          )
          None
        }
      })
  val genesisBonds: Map[Array[Byte], Int] = bondsFile match {
    case Some(file) =>
      Try {
        Source
          .fromFile(file)
          .getLines()
          .map(line => {
            val Array(pk, stake) = line.trim.split(" ")
            Base16.decode(pk) -> (stake.toInt)
          })
          .toMap
      }.getOrElse({
        Log[Task].warn(
          s"Specified bonds file ${file.getPath} cannot be parsed. Falling back on generating random validators."
        )
        newValidators
      })
    case None => newValidators
  }
  implicit val turanOracleEffect: SafetyOracle[Effect] = SafetyOracle.turanOracle[Effect]
  implicit val casperEffect: MultiParentCasper[Effect] = MultiParentCasper.hashSetCasper[Effect](
    storagePath,
    storageSize,
    genesisBlock(genesisBonds)
  )
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
          diagnostics.storeMetrics[Effect](runtime.space.store, conf.data_dir().normalize)
        GrpcServer
          .acquireServer[Effect](conf.grpcPort(), runtime)
      }
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
        diagnostics.storeMetrics[Task](resources.runtime.space.store, conf.data_dir().normalize)
      scheduler.scheduleAtFixedRate(10.seconds, 10.second)(StoreMetrics.report[Task].unsafeRunSync)
    }

  def addShutdownHook(resources: Resources): Task[Unit] =
    Task.delay(sys.addShutdownHook(clearResources(resources)))

  private def exit0: Task[Unit] = Task.delay(System.exit(0))

  private def newValidators: Map[Array[Byte], Int] = {
    val numValidators  = conf.numValidators.toOption.getOrElse(5)
    val keys           = Vector.fill(numValidators)(Ed25519.newKeyPair)
    val (_, pubKeys)   = keys.unzip
    val validatorsPath = conf.data_dir().resolve("validators")
    validatorsPath.toFile.mkdir()

    keys.foreach { //create files showing the secret key for each public key
      case (sec, pub) =>
        val sk      = Base16.encode(sec)
        val pk      = Base16.encode(pub)
        val skFile  = validatorsPath.resolve(s"$pk.sk").toFile
        val printer = new PrintWriter(skFile)
        printer.println(sk)
        printer.close()
    }

    val bonds        = pubKeys.zip((1 to numValidators).toVector).toMap
    val genBondsFile = validatorsPath.resolve(s"bonds.txt").toFile

    //create bonds file for editing/future use
    val printer = new PrintWriter(genBondsFile)
    bonds.foreach {
      case (pub, stake) =>
        val pk = Base16.encode(pub)
        Log[Task].info(s"Created validator $pk with bond $stake")
        printer.println(s"$pk $stake")
    }
    printer.close()

    bonds
  }

  def handleCommunications: ProtocolMessage => Effect[CommunicationResponse] =
    pm =>
      NodeDiscovery[Effect].handleCommunications(pm) >>= {
        case NotHandled => p2p.Network.dispatch[Effect](pm)
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
              if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
              else
                conf.bootstrap.toOption
                  .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                  .toEffect >>= (addr => p2p.Network.connectToBootstrap[Effect](addr)))
      _ <- if (res.isRight) MonadOps.forever(p2p.Network.findAndConnect[Effect], 0)
          else ().pure[Effect]
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
