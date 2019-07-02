package coop.rchain.casper.helper

import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path}

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, Resource, Sync}
import cats.implicits._
import cats.{Applicative, ApplicativeError, Id}
import coop.rchain.blockstorage._
import coop.rchain.casper.CasperState.CasperStateCell
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.MultiParentCasperTestUtil.GenesisContext
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
import coop.rchain.casper.helper.BlockDagStorageTestFixture.mapSize
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.TestNetwork.TestNetwork
import coop.rchain.casper.util.comm.{CasperPacketHandler, _}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.shared._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertions

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.util.Random

class HashSetCasperTestNode[F[_]](
    name: String,
    val local: PeerNode,
    tle: TransportLayerTestImpl[F],
    tls: TransportLayerServerTestImpl[F],
    val genesis: BlockMessage,
    sk: PrivateKey,
    logicalTime: LogicalTime[F],
    val blockDagDir: Path,
    val blockStoreDir: Path,
    blockProcessingLock: Semaphore[F],
    shardId: String = "rchain",
    val runtimeManager: RuntimeManager[F]
)(
    implicit concurrentF: Concurrent[F],
    val blockStore: BlockStore[F],
    val blockDagStorage: BlockDagStorage[F],
    val metricEff: Metrics[F],
    val casperState: CasperStateCell[F]
) {

  implicit val logEff                       = new LogStub[F](Log.log[F])
  implicit val timeEff                      = logicalTime
  implicit val connectionsCell              = Cell.unsafe[F, Connections](Connect.Connections.empty)
  implicit val transportLayerEff            = tle
  implicit val cliqueOracleEffect           = SafetyOracle.cliqueOracle[F]
  implicit val lastFinalizedBlockCalculator = LastFinalizedBlockCalculator[F](0f)
  implicit val rpConfAsk                    = createRPConfAsk[F](local)

  // Scalatest `assert` macro needs some member of the Assertions trait.
  // An (inferior) alternative would be to inherit the trait...
  private val scalatestAssertions = new Assertions {}
  import scalatestAssertions._

  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)

  val validatorId = ValidatorIdentity(Secp256k1.toPublic(sk), sk, "secp256k1")

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

  val engine                             = new Running(casperEff, approvedBlock, ().pure[F])
  implicit val engineCell: EngineCell[F] = Cell.unsafe[F, Engine[F]](engine)
  implicit val packetHandlerEff          = CasperPacketHandler[F]

  val span = new NoopSpan[F]

  def addBlock(deployDatums: DeployData*): F[BlockMessage] =
    addBlockStatus(Valid)(deployDatums: _*)

  def addBlockStatus(expectedStatus: BlockStatus)(deployDatums: DeployData*): F[BlockMessage] =
    for {
      block  <- createBlock(deployDatums: _*)
      status <- casperEff.addBlock(block, ignoreDoppelgangerCheck[F])
      _      = assert(status == expectedStatus)
    } yield block

  def createBlock(deployDatums: DeployData*): F[BlockMessage] =
    for {
      _                 <- deployDatums.toList.traverse(casperEff.deploy)
      createBlockResult <- casperEff.createBlock
      Created(block)    = createBlockResult
    } yield block

  def receive(): F[Unit] = tls.receive(p => handle[F](p), kp(().pure[F])).void
}

object HashSetCasperTestNode {
  type Effect[A] = Task[A]

  import coop.rchain.catscontrib._

  def standaloneEff(
      genesis: GenesisContext,
      storageSize: Long = 1024L * 1024 * 10
  )(
      implicit scheduler: Scheduler
  ): Resource[Effect, HashSetCasperTestNode[Effect]] =
    networkEff(
      genesis,
      networkSize = 1,
      storageSize = storageSize
    ).map(_.head)

  def networkEff(genesis: GenesisContext, networkSize: Int, storageSize: Long = 1024L * 1024 * 10)(
      implicit scheduler: Scheduler
  ): Resource[Effect, IndexedSeq[HashSetCasperTestNode[Effect]]] =
    networkF[Effect](
      genesis.validatorSks.take(networkSize).toVector,
      genesis.genesisBlock,
      genesis.storageDirectory,
      storageSize,
      createRuntime
    )(
      Concurrent[Effect],
      TestNetwork.empty[Effect]
    )

  private def createRuntime(storageDirectory: Path, storageSize: Long)(
      implicit scheduler: Scheduler
  ): Resource[Effect, RuntimeManager[Effect]] = {
    implicit val log                       = Log.log[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    val activeRuntime =
      Runtime
        .createWithEmptyCost[Task, Task.Par](storageDirectory, storageSize)
        .unsafeRunSync
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime).unsafeRunSync
    Resource.make[Effect, RuntimeManager[Effect]](
      runtimeManager.pure[Effect]
    )(_ => activeRuntime.close())
  }

  private def networkF[F[_]](
      sks: IndexedSeq[PrivateKey],
      genesis: BlockMessage,
      storageMatrixPath: Path,
      storageSize: Long,
      createRuntime: (Path, Long) => Resource[F, RuntimeManager[F]]
  )(
      implicit concurrentF: Concurrent[F],
      testNetworkF: TestNetwork[F]
  ): Resource[F, IndexedSeq[HashSetCasperTestNode[F]]] = {
    val n     = sks.length
    val names = (1 to n).map(i => s"node-$i")
    val peers = names.map(peerNode(_, 40400))

    val logicalTime: LogicalTime[F] = new LogicalTime[F]

    val nodesF =
      names
        .zip(peers)
        .zip(sks)
        .toList
        .traverse {
          case ((name, currentPeerNode), sk) =>
            createNode(
              name,
              currentPeerNode,
              genesis,
              sk,
              storageMatrixPath,
              storageSize,
              logicalTime,
              createRuntime
            )
        }
        .map(_.toVector)

    nodesF.flatMap { nodes =>
      import Connections._
      //make sure all nodes know about each other
      Resource.liftF(
        for {
          _ <- ().pure[F]
          pairs = for {
            n <- nodes
            m <- nodes
            if n.local != m.local
          } yield (n, m)
          _ <- pairs.foldLeft(().pure[F]) {
                case (f, (n, m)) =>
                  f.flatMap(
                    _ =>
                      n.connectionsCell.flatModify(
                        _.addConn[F](m.local)
                      )
                  )
              }
        } yield nodes
      )
    }
  }

  private def createNode[F[_]: Concurrent: TestNetwork](
      name: String,
      currentPeerNode: PeerNode,
      genesis: BlockMessage,
      sk: PrivateKey,
      storageMatrixPath: Path,
      storageSize: Long,
      logicalTime: LogicalTime[F],
      createRuntime: (Path, Long) => Resource[F, RuntimeManager[F]]
  ): Resource[F, HashSetCasperTestNode[F]] = {
    val tle                = new TransportLayerTestImpl[F]()
    val tls                = new TransportLayerServerTestImpl[F](currentPeerNode)
    implicit val log       = Log.log[F]
    implicit val metricEff = new Metrics.MetricsNOP[F]
    for {
      storageDirectory <- Resource.make[F, Path](
                           Sync[F].delay {
                             val dir = Files.createTempDirectory(s"hash-set-casper-test-$name")
                             copyDir(storageMatrixPath, dir)
                           }
                         )(dir => Sync[F].delay { dir.recursivelyDelete() })

      blockStoreDir = storageDirectory.resolve("block-store")
      blockStore <- Resource.make[F, BlockStore[F]](
                     BlockDagStorageTestFixture.createBlockStorage(blockStoreDir)
                   )(_.close())

      blockDagDir = storageDirectory.resolve("block-dag-store")
      blockDagStorage <- Resource.make[F, BlockDagStorage[F]](
                          BlockDagFileStorage
                            .create[F](makeBlockDagFileStorageConfig(blockDagDir))
                            .widen
                        )(_.close())

      rspaceDir      = storageDirectory.resolve("rspace")
      runtimeManager <- createRuntime(rspaceDir, storageSize)

      node <- Resource.make[F, HashSetCasperTestNode[F]] {
               for {
                 _                   <- TestNetwork.addPeer(currentPeerNode)
                 blockProcessingLock <- Semaphore[F](1)
                 casperState         <- Cell.mvarCell[F, CasperState](CasperState())
                 node = new HashSetCasperTestNode[F](
                   name,
                   currentPeerNode,
                   tle,
                   tls,
                   genesis,
                   sk,
                   logicalTime,
                   blockDagDir,
                   blockStoreDir,
                   blockProcessingLock,
                   "rchain",
                   runtimeManager
                 )(
                   Concurrent[F],
                   blockStore,
                   blockDagStorage,
                   metricEff,
                   casperState
                 )
               } yield node
             }(_ => ().pure[F])
    } yield node
  }

  private def copyDir(src: Path, dest: Path): Path = {
    Files
      .walk(src)
      .forEach(source => Files.copy(source, dest.resolve(src.relativize(source)), REPLACE_EXISTING))
    dest
  }

  def makeBlockDagFileStorageConfig(blockDagDir: Path) =
    BlockDagFileStorage.Config(
      blockDagDir.resolve("latest-messages-data"),
      blockDagDir.resolve("latest-messages-crc"),
      blockDagDir.resolve("block-metadata-data"),
      blockDagDir.resolve("block-metadata-crc"),
      blockDagDir.resolve("equivocations-tracker-data"),
      blockDagDir.resolve("equivocations-tracker-crc"),
      blockDagDir.resolve("invalid-blocks-data"),
      blockDagDir.resolve("invalid-blocks-crc"),
      blockDagDir.resolve("checkpoints"),
      blockDagDir.resolve("block-number-index"),
      mapSize
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
