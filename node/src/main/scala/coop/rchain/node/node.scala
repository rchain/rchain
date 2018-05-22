package coop.rchain.node

import java.io.{File, PrintWriter}
import java.net.SocketAddress
import java.util.UUID
import io.grpc.{Server, ServerBuilder}

import coop.rchain.shared.StringOps._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.genesisBlock
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.comm._, CommError._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p
import coop.rchain.p2p.Network.KeysStore
import coop.rchain.p2p.effects._
import coop.rchain.rholang.interpreter.Runtime
import monix.eval.Task
import monix.execution.Scheduler

import scala.io.Source
import scala.util.Try

class NodeRuntime(conf: Conf)(implicit scheduler: Scheduler) {

  implicit class ThrowableOps(th: Throwable) {
    def containsMessageWith(str: String): Boolean =
      if (th.getCause() == null) th.getMessage.contains(str)
      else th.getMessage.contains(str) || th.getCause().containsMessageWith(str)
  }

  import ApplicativeError_._

  /** Configuration */
  private val host           = conf.fetchHost()
  private val name           = conf.name.toOption.fold(UUID.randomUUID.toString.replaceAll("-", ""))(id)
  private val address        = s"rnode://$name@$host:${conf.port()}"
  private val src            = p2p.NetworkAddress.parse(address).right.get
  private val remoteKeysPath = conf.data_dir().resolve("keys").resolve(s"${name}-rnode-remote.keys")
  private val keysPath       = conf.data_dir().resolve("keys").resolve(s"${name}-rnode.keys")
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
  implicit val encryptionEffect: Encryption[Task]           = effects.encryption(keysPath)
  implicit val logEffect: Log[Task]                         = effects.log
  implicit val timeEffect: Time[Task]                       = effects.time
  implicit val metricsEffect: Metrics[Task]                 = effects.metrics
  implicit val inMemoryPeerKeysEffect: KeysStore[Task]      = effects.remoteKeysKvs(remoteKeysPath)
  val net                                                   = new UnicastNetwork(src)
  implicit val nodeDiscoveryEffect: NodeDiscovery[Effect]   = effects.nodeDiscovery[Effect](net)
  implicit val transportLayerEffect: TransportLayer[Effect] = effects.transportLayer[Effect](net)

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

  implicit val casperEffect: MultiParentCasper[Effect] = MultiParentCasper.hashSetCasper[Effect](
    storagePath,
    storageSize,
    genesisBlock(genesisBonds)
  )
  implicit val packetHandlerEffect: PacketHandler[Effect] = effects.packetHandler[Effect](
    casperPacketHandler[Effect]
  )

  case class Resources(grpcServer: Server,
                       metricsServer: MetricsServer,
                       httpServer: HttpServer,
                       runtime: Runtime)

  def aquireResources: Effect[Resources] =
    for {
      runtime <- Runtime.create(storagePath, storageSize).pure[Effect]
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
    printer.close

    bonds
  }

  private def receiveAndDispatch: Effect[Unit] =
    TransportLayer[Effect].receive >>= {
      case None      => ().pure[Effect]
      case Some(msg) => p2p.Network.dispatch[Effect](msg)
    }

  private def unrecoverableNodeProgram: Effect[Unit] =
    for {
      _         <- Log[Effect].info(s"  ============= RChain Node ${BuildInfo.version} ============".blue)
      resources <- aquireResources
      _         <- startResources(resources)
      _         <- addShutdownHook(resources).toEffect
      // TODO handle errors on receive (currently ignored)
      _ <- receiveAndDispatch.value.void.forever.executeAsync.start.toEffect
      _ <- Log[Effect].info(s"Listening for traffic on $address.")
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
        th.getStackTrace().toList.traverse(ste => Log[Task].error(ste.toString))
    } *> exit0.as(Right(())))
}
