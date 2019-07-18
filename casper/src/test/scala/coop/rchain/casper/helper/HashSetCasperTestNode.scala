package coop.rchain.casper.helper

import java.nio.file.Path

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, Resource}
import cats.implicits._
import coop.rchain.blockstorage._
import coop.rchain.casper.CasperState.CasperStateCell
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.casper.helper.BlockDagStorageTestFixture.mapSize
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.TestNetwork.TestNetwork
import coop.rchain.casper.util.comm.{CasperPacketHandler, _}
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertions

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

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
    shardId: String = "rchain"
)(
    implicit concurrentF: Concurrent[F],
    val blockStore: BlockStore[F],
    val blockDagStorage: BlockDagStorage[F],
    val metricEff: Metrics[F],
    val span: Span[F],
    val casperState: CasperStateCell[F],
    val runtimeManager: RuntimeManager[F]
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

  implicit val requestedBlocks: Running.RequestedBlocks[F] =
    Cell.unsafe[F, Map[BlockHash, Running.Requested]](Map.empty[BlockHash, Running.Requested])

  implicit val casperEff = new MultiParentCasperImpl[F](
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
      Resources.mkRuntimeManagerAt[Effect](_)(storageSize)
    )(
      Concurrent[Effect],
      TestNetwork.empty[Effect]
    )

  private def networkF[F[_]: Concurrent: TestNetwork](
      sks: IndexedSeq[PrivateKey],
      genesis: BlockMessage,
      storageMatrixPath: Path,
      createRuntime: Path => Resource[F, RuntimeManager[F]]
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
      logicalTime: LogicalTime[F],
      createRuntime: Path => Resource[F, RuntimeManager[F]]
  ): Resource[F, HashSetCasperTestNode[F]] = {
    val tle                = new TransportLayerTestImpl[F]()
    val tls                = new TransportLayerServerTestImpl[F](currentPeerNode)
    implicit val log       = Log.log[F]
    implicit val metricEff = new Metrics.MetricsNOP[F]
    implicit val spanEff   = NoopSpan[F]()
    for {
      paths <- Resources.copyStorage[F](storageMatrixPath)

      blockStore <- Resource.make[F, BlockStore[F]](
                     BlockDagStorageTestFixture.createBlockStorage(paths.blockStoreDir)
                   )(_.close())

      blockDagStorage <- Resource.make[F, BlockDagStorage[F]](
                          BlockDagFileStorage
                            .create[F](makeBlockDagFileStorageConfig(paths.blockDagDir))
                            .widen
                        )(_.close())

      runtimeManager <- createRuntime(paths.rspaceDir)

      node <- Resource.liftF(
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
                   paths.blockDagDir,
                   paths.blockStoreDir,
                   blockProcessingLock,
                   "rchain"
                 )(
                   Concurrent[F],
                   blockStore,
                   blockDagStorage,
                   metricEff,
                   spanEff,
                   casperState,
                   runtimeManager
                 )
               } yield node
             )
    } yield node
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

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

}
