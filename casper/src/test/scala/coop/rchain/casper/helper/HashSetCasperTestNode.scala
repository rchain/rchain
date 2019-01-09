package coop.rchain.casper.helper

import java.nio.file.{Files, Path}

import cats.data.EitherT
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, ApplicativeError, Id, Monad}
import coop.rchain.blockstorage._
import coop.rchain.catscontrib._
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
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.{Cell, Log, StoreType}
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task
import monix.execution.Scheduler

import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.util.Random

class HashSetCasperTestNode[F[_]](
    name: String,
    val local: PeerNode,
    tle: TransportLayerTestImpl[F],
    val genesis: BlockMessage,
    sk: Array[Byte],
    logicalTime: LogicalTime[F],
    implicit val errorHandlerEff: ErrorHandler[F],
    storageSize: Long,
    val blockDagDir: Path,
    val blockStoreDir: Path,
    blockProcessingLock: Semaphore[F],
    shardId: String = "rchain"
)(
    implicit scheduler: Scheduler,
    syncF: Sync[F],
    captureF: Capture[F],
    concurrentF: Concurrent[F],
    val blockStore: BlockStore[F],
    val blockDagStorage: BlockDagStorage[F],
    val metricEff: Metrics[F],
    val abF: ToAbstractContext[F]
) {

  private val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-$name")

  implicit val logEff            = new LogStub[F]
  implicit val timeEff           = logicalTime
  implicit val connectionsCell   = Cell.unsafe[F, Connections](Connect.Connections.empty)
  implicit val transportLayerEff = tle
  implicit val turanOracleEffect = SafetyOracle.turanOracle[F]
  implicit val rpConfAsk         = createRPConfAsk[F](local)

  val activeRuntime =
    Runtime.create[Task, Task.Par](storageDirectory, storageSize, StoreType.LMDB).unsafeRunSync
  val runtimeManager                 = RuntimeManager.fromRuntime(activeRuntime)
  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)

  val validatorId = ValidatorIdentity(Ed25519.toPublic(sk), sk, "ed25519")

  val approvedBlock = ApprovedBlock(candidate = Some(ApprovedBlockCandidate(block = Some(genesis))))

  implicit val labF        = LastApprovedBlock.unsafe[F](Some(approvedBlock))
  val postGenesisStateHash = ProtoUtil.postStateHash(genesis)

  implicit val casperEff = new MultiParentCasperImpl[F](
    runtimeManager,
    Some(validatorId),
    genesis,
    postGenesisStateHash,
    shardId,
    blockProcessingLock
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
      blockDagStorage.getRepresentation.flatMap { dag =>
        InterpreterUtil
          .validateBlockCheckpoint[F](
            genesis,
            dag,
            runtimeManager
          )
          .void
      }

  def receive(): F[Unit] = tle.receive(p => handle[F](p, defaultTimeout), kp(().pure[F]))

  def tearDown(): Unit = {
    tearDownNode()
    blockStoreDir.recursivelyDelete()
    blockDagDir.recursivelyDelete()
  }

  def tearDownNode(): Unit = {
    activeRuntime.close().unsafeRunSync
    blockStore.close()
    blockDagStorage.close()
  }
}

object HashSetCasperTestNode {
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  import coop.rchain.catscontrib._

  implicit val absF = new ToAbstractContext[Effect] {
    def fromTask[A](fa: Task[A]): Effect[A] = new MonadOps(fa).liftM[CommErrT]
  }

  def standaloneF[F[_]](
      genesis: BlockMessage,
      sk: Array[Byte],
      storageSize: Long = 1024L * 1024 * 10
  )(
      implicit scheduler: Scheduler,
      errorHandler: ErrorHandler[F],
      syncF: Sync[F],
      captureF: Capture[F],
      concurrentF: Concurrent[F],
      absF: ToAbstractContext[F]
  ): F[HashSetCasperTestNode[F]] = {
    val name     = "standalone"
    val identity = peerNode(name, 40400)
    val tle =
      new TransportLayerTestImpl[F](identity, Map.empty[PeerNode, Ref[F, mutable.Queue[Protocol]]])
    val logicalTime: LogicalTime[F] = new LogicalTime[F]
    implicit val log                = new Log.NOPLog[F]()
    implicit val metricEff          = new Metrics.MetricsNOP[F]

    val blockDagDir   = BlockDagStorageTestFixture.blockDagStorageDir
    val blockStoreDir = BlockDagStorageTestFixture.blockStorageDir
    implicit val blockStore =
      LMDBBlockStore.create[F](
        LMDBBlockStore.Config(path = blockStoreDir, mapSize = storageSize)
      )
    for {
      blockDagStorage <- BlockDagFileStorage.createEmptyFromGenesis[F](
                          BlockDagFileStorage.Config(
                            blockDagDir.resolve("latest-messages-data"),
                            blockDagDir.resolve("latest-messages-crc"),
                            blockDagDir.resolve("block-metadata-data"),
                            blockDagDir.resolve("block-metadata-crc"),
                            blockDagDir.resolve("checkpoints")
                          ),
                          genesis
                        )
      blockProcessingLock <- Semaphore[F](1)
      node = new HashSetCasperTestNode[F](
        name,
        identity,
        tle,
        genesis,
        sk,
        logicalTime,
        errorHandler,
        storageSize,
        blockDagDir,
        blockStoreDir,
        blockProcessingLock,
        "rchain"
      )(scheduler, syncF, captureF, concurrentF, blockStore, blockDagStorage, metricEff, absF)
      result <- node.initialize.map(_ => node)
    } yield result
  }
  def standaloneEff(genesis: BlockMessage, sk: Array[Byte], storageSize: Long = 1024L * 1024 * 10)(
      implicit scheduler: Scheduler
  ): HashSetCasperTestNode[Effect] =
    standaloneF[Effect](genesis, sk, storageSize)(
      scheduler,
      ApplicativeError_[Effect, CommError],
      syncEffectInstance,
      Capture[Effect],
      Concurrent[Effect],
      ToAbstractContext[Effect]
    ).value.unsafeRunSync.right.get

  def networkF[F[_]](
      sks: IndexedSeq[Array[Byte]],
      genesis: BlockMessage,
      storageSize: Long = 1024L * 1024 * 10
  )(
      implicit scheduler: Scheduler,
      errorHandler: ErrorHandler[F],
      syncF: Sync[F],
      captureF: Capture[F],
      concurrentF: Concurrent[F],
      absF: ToAbstractContext[F]
  ): F[IndexedSeq[HashSetCasperTestNode[F]]] = {
    val n     = sks.length
    val names = (1 to n).map(i => s"node-$i")
    val peers = names.map(peerNode(_, 40400))
    val msgQueues = peers
      .map(_ -> new mutable.Queue[Protocol]())
      .toMap
      .mapValues(Ref.unsafe[F, mutable.Queue[Protocol]])
    val logicalTime: LogicalTime[F] = new LogicalTime[F]

    val nodesF =
      names
        .zip(peers)
        .zip(sks)
        .toList
        .traverse {
          case ((n, p), sk) =>
            val tle                = new TransportLayerTestImpl[F](p, msgQueues)
            implicit val log       = new Log.NOPLog[F]()
            implicit val metricEff = new Metrics.MetricsNOP[F]

            val blockDagDir   = BlockDagStorageTestFixture.blockDagStorageDir
            val blockStoreDir = BlockDagStorageTestFixture.blockStorageDir
            implicit val blockStore =
              LMDBBlockStore.create[F](
                LMDBBlockStore.Config(path = blockStoreDir, mapSize = storageSize)
              )
            for {
              blockDagStorage <- BlockDagFileStorage.createEmptyFromGenesis[F](
                                  BlockDagFileStorage.Config(
                                    blockDagDir.resolve("latest-messages-data"),
                                    blockDagDir.resolve("latest-messages-crc"),
                                    blockDagDir.resolve("block-metadata-data"),
                                    blockDagDir.resolve("block-metadata-crc"),
                                    blockDagDir.resolve("checkpoints")
                                  ),
                                  genesis
                                )
              semaphore <- Semaphore[F](1)
              node = new HashSetCasperTestNode[F](
                n,
                p,
                tle,
                genesis,
                sk,
                logicalTime,
                errorHandler,
                storageSize,
                blockDagDir,
                blockStoreDir,
                semaphore,
                "rchain"
              )(
                scheduler,
                syncF,
                captureF,
                concurrentF,
                blockStore,
                blockDagStorage,
                metricEff,
                absF
              )
            } yield node
        }
        .map(_.toVector)

    import Connections._
    //make sure all nodes know about each other
    for {
      nodes <- nodesF
      pairs = for {
        n <- nodes
        m <- nodes
        if n.local != m.local
      } yield (n, m)
      _ <- nodes.traverse(_.initialize).void
      _ <- pairs.foldLeft(().pure[F]) {
            case (f, (n, m)) =>
              f.flatMap(
                _ =>
                  n.connectionsCell.modify(_.addConn[F](m.local)(Monad[F], n.logEff, n.metricEff))
              )
          }
    } yield nodes
  }
  def networkEff(
      sks: IndexedSeq[Array[Byte]],
      genesis: BlockMessage,
      storageSize: Long = 1024L * 1024 * 10
  )(implicit scheduler: Scheduler): Effect[IndexedSeq[HashSetCasperTestNode[Effect]]] =
    networkF[Effect](sks, genesis, storageSize)(
      scheduler,
      ApplicativeError_[Effect, CommError],
      syncEffectInstance,
      Capture[Effect],
      Concurrent[Effect],
      ToAbstractContext[Effect]
    )

  val appErrId = new ApplicativeError[Id, CommError] {
    def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = Applicative[Id].ap[A, B](ff)(fa)
    def pure[A](x: A): Id[A]                       = Applicative[Id].pure[A](x)
    def raiseError[A](e: CommError): Id[A] = {
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

      throw new Exception(errString)
    }

    def handleErrorWith[A](fa: Id[A])(f: (CommError) => Id[A]): Id[A] = fa
  }

  implicit val syncEffectInstance = cats.effect.Sync.catsEitherTSync[Task, CommError]

  val errorHandler = ApplicativeError_.applicativeError[Id, CommError](appErrId)

  def randomBytes(length: Int): Array[Byte] = Array.fill(length)(Random.nextInt(256).toByte)

  def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
