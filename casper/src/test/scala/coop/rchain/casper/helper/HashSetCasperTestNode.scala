package coop.rchain.casper.helper

import java.nio.file.Files

import cats.{Applicative, ApplicativeError, Id, Monad, Parallel, Traverse}
import cats.data.EitherT
import cats.effect.IO.ContextSwitch
import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.blockstorage.{BlockMetadata, LMDBBlockStore}
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CasperPacketHandler.{
  ApprovedBlockReceivedHandler,
  CasperPacketHandlerImpl,
  CasperPacketHandlerInternal
}
import coop.rchain.casper.util.comm.TransportLayerTestImpl
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Cell
import coop.rchain.shared.PathOps.RichPath
import monix.execution.Scheduler

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Random
import coop.rchain.shared.{Cell, Time}
import monix.eval.Task

import scala.concurrent.ExecutionContext

class HashSetCasperTestNode[F[_]](
    val activeRuntime: Runtime[F],
    val runtimeManager: RuntimeManager[F],
    name: String,
    val local: PeerNode,
    tle: TransportLayerTestImpl[F],
    val genesis: BlockMessage,
    sk: Array[Byte],
    logicalTime: LogicalTime[F],
    implicit val errorHandlerEff: ErrorHandler[F],
    storageSize: Long,
    shardId: String = "rchain"
)(
    implicit
    syncF: Sync[F],
    captureF: Capture[F]
) {

  implicit val logEff            = new LogStub[F]
  implicit val timeEff           = logicalTime
  implicit val connectionsCell   = Cell.unsafe[F, Connections](Connect.Connections.empty)
  implicit val transportLayerEff = tle
  implicit val metricEff         = new Metrics.MetricsNOP[F]
  val dir                        = BlockStoreTestFixture.dbDir
  implicit val blockStore =
    LMDBBlockStore.create[F](LMDBBlockStore.Config(path = dir, mapSize = storageSize))
  implicit val turanOracleEffect = SafetyOracle.turanOracle[F]
  implicit val rpConfAsk         = createRPConfAsk[F](local)

  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)

  val validatorId = ValidatorIdentity(Ed25519.toPublic(sk), sk, "ed25519")

  val approvedBlock = ApprovedBlock(candidate = Some(ApprovedBlockCandidate(block = Some(genesis))))

  implicit val labF = LastApprovedBlock.unsafe[F](Some(approvedBlock))

  val genesisBonds          = ProtoUtil.bonds(genesis)
  val initialLatestMessages = genesisBonds.map(_.validator -> genesis).toMap
  val dag = BlockDag.empty.copy(
    latestMessages = initialLatestMessages,
    dataLookup = Map(genesis.blockHash -> BlockMetadata.fromBlock(genesis)),
    topoSort = Vector(Vector(genesis.blockHash))
  )
  val postGenesisStateHash = genesis.body.get.postState.get.tuplespace
  implicit val casperEff = new MultiParentCasperImpl[F](
    runtimeManager,
    Some(validatorId),
    genesis,
    dag,
    postGenesisStateHash,
    shardId
  )

  implicit val multiparentCasperRef = MultiParentCasperRef.unsafe[F](Some(casperEff))

  val handlerInternal = new ApprovedBlockReceivedHandler(casperEff, approvedBlock)
  val casperPacketHandler =
    new CasperPacketHandlerImpl[F](Ref.unsafe[F, CasperPacketHandlerInternal[F]](handlerInternal))
  implicit val packetHandlerEff = PacketHandler.pf[F](
    casperPacketHandler.handle
  )

  def initialize(): F[Unit] =
    // pre-population removed from internals of Casper
    blockStore.put(genesis.blockHash, genesis) *>
      InterpreterUtil
        .validateBlockCheckpoint[F](
          genesis,
          dag,
          runtimeManager
        )
        .void

  def receive(): F[Unit] = tle.receive(p => handle[F](p, defaultTimeout), kp(().pure[F]))

  def tearDown(): F[Unit] =
    for {
      _ <- tearDownNode()
      _ <- Sync[F].delay { dir.recursivelyDelete() }
    } yield ()

  def tearDownNode(): F[Unit] =
    for {
      _ <- activeRuntime.close()
      _ <- blockStore.close()
    } yield ()
}

object HashSetCasperTestNode {
  type Effect[A] = EitherT[Task, CommError, A]

  def create[M[_], F[_]](
      name: String,
      local: PeerNode,
      tle: TransportLayerTestImpl[M],
      genesis: BlockMessage,
      sk: Array[Byte],
      logicalTime: LogicalTime[M],
      errorHandlerEff: ErrorHandler[M],
      storageSize: Long,
      shardId: String = "rchain"
  )(
      implicit
      scheduler: Scheduler,
      syncF: Sync[M],
      parallel: Parallel[M, F],
      contextShift: ContextShift[M],
      executionContext: ExecutionContext,
      capture: Capture[M]
  ): M[HashSetCasperTestNode[M]] =
    for {

      storageDirectory <- Sync[M].delay { Files.createTempDirectory(s"hash-set-casper-test-$name") }
      activeRuntime    <- Runtime.create[M, F](storageDirectory, storageSize)
      runtimeManager   <- RuntimeManager.fromRuntime[M](activeRuntime)
    } yield
      new HashSetCasperTestNode[M](
        activeRuntime,
        runtimeManager,
        name,
        local,
        tle,
        genesis,
        sk,
        logicalTime,
        errorHandlerEff,
        storageSize,
        shardId
      )
  def standaloneF[M[_], F[_]](
      genesis: BlockMessage,
      sk: Array[Byte],
      storageSize: Long = 1024L * 1024 * 10
  )(
      implicit scheduler: Scheduler,
      errorHandler: ErrorHandler[M],
      syncF: Sync[M],
      parallel: Parallel[M, F],
      contextShift: ContextShift[M],
      captureF: Capture[M]
  ): M[HashSetCasperTestNode[M]] = {
    val name     = "standalone"
    val identity = peerNode(name, 40400)
    val tle =
      new TransportLayerTestImpl[M](identity, Map.empty[PeerNode, Ref[M, mutable.Queue[Protocol]]])
    val logicalTime: LogicalTime[M] = new LogicalTime[M]

    for {
      node <- HashSetCasperTestNode.create[M, F](
               name,
               identity,
               tle,
               genesis,
               sk,
               logicalTime,
               errorHandler,
               storageSize
             )
      _ <- node.initialize
    } yield node
  }

  def standalone(genesis: BlockMessage, sk: Array[Byte], storageSize: Long = 1024L * 1024 * 10)(
      implicit scheduler: Scheduler
  ): HashSetCasperTestNode[Task] = {
    implicit val errorHandlerEff = errorHandler
    standaloneF[Task, Task.Par](genesis, sk, storageSize).runSyncUnsafe(10.seconds)
  }
  def standaloneEff(genesis: BlockMessage, sk: Array[Byte], storageSize: Long = 1024L * 1024 * 10)(
      implicit scheduler: Scheduler
  ): HashSetCasperTestNode[Effect] =
    standaloneF[Effect, Effect](genesis, sk, storageSize)(
      scheduler,
      ApplicativeError_[Effect, CommError],
      syncEffectInstance,
      Parallel.identity[Effect],
      implicitly,
      Capture[Effect]
    ).value.unsafeRunSync.right.get

  def networkF[M[_], F[_]](
      sks: IndexedSeq[Array[Byte]],
      genesis: BlockMessage,
      storageSize: Long = 1024L * 1024 * 10
  )(
      implicit scheduler: Scheduler,
      errorHandler: ErrorHandler[M],
      syncM: Sync[M],
      parallel: Parallel[M, F],
      contextShift: ContextShift[M],
      captureM: Capture[M]
  ): M[IndexedSeq[HashSetCasperTestNode[M]]] = {
    val n     = sks.length
    val names = (1 to n).map(i => s"node-$i")
    val peers = names.map(peerNode(_, 40400))
    val msgQueues = peers
      .map(_ -> new mutable.Queue[Protocol]())
      .toMap
      .mapValues(Ref.unsafe[M, mutable.Queue[Protocol]])
    val logicalTime: LogicalTime[M] = new LogicalTime[M]

    import Connections._

    for {
      nodes <- names
                .zip(peers)
                .zip(sks)
                .toStream
                .traverse {
                  case ((n, p), sk) =>
                    val tle = new TransportLayerTestImpl[M](p, msgQueues)
                    HashSetCasperTestNode.create(
                      n,
                      p,
                      tle,
                      genesis,
                      sk,
                      logicalTime,
                      errorHandler,
                      storageSize
                    )
                }
                .map(_.toVector)

      //make sure all nodes know about each other
      pairs = for {
        n <- nodes
        m <- nodes
        if n.local != m.local
      } yield (n, m)

      _ <- nodes.traverse(_.initialize).void
      _ <- pairs.foldLeft(().pure[M]) {
            case (f, (n, m)) =>
              f.flatMap(
                _ =>
                  n.connectionsCell.modify(_.addConn[M](m.local)(Monad[M], n.logEff, n.metricEff))
              )
          }
    } yield nodes
  }

  def network(
      sks: IndexedSeq[Array[Byte]],
      genesis: BlockMessage,
      storageSize: Long = 1024L * 1024 * 10
  )(implicit scheduler: Scheduler): IndexedSeq[HashSetCasperTestNode[Task]] = {
    implicit val errorHandlerEff = errorHandler
    networkF[Task, Task.Par](sks, genesis, storageSize).runSyncUnsafe(30.seconds)
  }
  def networkEff(
      sks: IndexedSeq[Array[Byte]],
      genesis: BlockMessage,
      storageSize: Long = 1024L * 1024 * 10
  )(implicit scheduler: Scheduler): Effect[IndexedSeq[HashSetCasperTestNode[Effect]]] =
    networkF[Effect, Effect](sks, genesis, storageSize)(
      scheduler,
      ApplicativeError_[Effect, CommError],
      syncEffectInstance,
      Parallel.identity[Effect],
      implicitly,
      Capture[Effect]
    )

  val appErrTask = new ApplicativeError[Task, CommError] {
    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] = Applicative[Task].ap[A, B](ff)(fa)
    def pure[A](x: A): Task[A]                           = Applicative[Task].pure[A](x)
    def raiseError[A](e: CommError): Task[A] = {
      val errString = e match {
        case UnknownCommError(msg)                => s"UnknownCommError($msg)"
        case DatagramSizeError(size)              => s"DatagramSizeError($size)"
        case DatagramFramingError(ex)             => s"DatagramFramingError($ex)"
        case DatagramException(ex)                => s"DatagramException($ex)"
        case HeaderNotAvailable                   => "HeaderNotAvailable"
        case ProtocolException(th)                => s"ProtocolException($th)"
        case UnknownProtocolError(msg)            => s"UnknownProtocolError($msg)"
        case PublicKeyNotAvailable(node)          => s"PublicKeyNotAvailable($node)"
        case ParseError(msg)                      => s"ParseError($msg)"
        case EncryptionHandshakeIncorrectlySigned => "EncryptionHandshakeIncorrectlySigned"
        case BootstrapNotProvided                 => "BootstrapNotProvided"
        case PeerNodeNotFound(peer)               => s"PeerNodeNotFound($peer)"
        case PeerUnavailable(peer)                => s"PeerUnavailable($peer)"
        case MalformedMessage(pm)                 => s"MalformedMessage($pm)"
        case CouldNotConnectToBootstrap           => "CouldNotConnectToBootstrap"
        case InternalCommunicationError(msg)      => s"InternalCommunicationError($msg)"
        case TimeOut                              => "TimeOut"
        case _                                    => e.toString
      }

      Task.raiseError(new Exception(errString))
    }

    def handleErrorWith[A](fa: Task[A])(f: (CommError) => Task[A]): Task[A] = fa
  }

  implicit val syncEffectInstance = SyncInstances.syncEffect[CommError](commError => {
    new Exception(s"CommError: $commError")
  }, e => { UnknownCommError(e.getMessage) })

  val errorHandler = ApplicativeError_.applicativeError[Task, CommError](appErrTask)

  def randomBytes(length: Int): Array[Byte] = Array.fill(length)(Random.nextInt(256).toByte)

  def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
