package coop.rchain.casper.helper

import java.net.URLEncoder
import java.nio.file.Path

import cats.Monad
import cats.data.State
import cats.effect.concurrent.{Ref, Semaphore}
import cats.effect.{Concurrent, Resource, Sync}
import cats.implicits._
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.{BlockDagFileStorage, BlockDagStorage}
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.{LastFinalizedFileStorage, LastFinalizedStorage}
import coop.rchain.casper
import casper.engine.BlockRetriever._
import coop.rchain.casper._
import coop.rchain.casper.api.{BlockAPI, GraphConfig, GraphzGenerator}
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
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
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.graphz.{Graphz, StringSerializer}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.Runtime.RhoHistoryRepository
import coop.rchain.shared._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertions

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

class TestNode[F[_]](
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
    synchronyConstraintThreshold: Double,
    maxNumberOfParents: Int = Estimator.UnlimitedParents,
    maxParentDepth: Option[Int] = None,
    shardId: String = "root",
    finalizationRate: Int = 1,
    isReadOnly: Boolean = false
)(
    implicit concurrentF: Concurrent[F],
    implicit val blockStore: BlockStore[F],
    implicit val blockDagStorage: BlockDagStorage[F],
    implicit val lastFinalizedStorage: LastFinalizedStorage[F],
    implicit val deployStorage: DeployStorage[F],
    val metricEff: Metrics[F],
    val span: Span[F],
    val casperBufferStorage: CasperBufferStorage[F],
    val runtimeManager: RuntimeManager[F],
    val rhoHistoryRepository: RhoHistoryRepository[F]
) {

  implicit val logEff                       = new LogStub[F](Log.log[F])
  implicit val timeEff                      = logicalTime
  implicit val connectionsCell              = Cell.unsafe[F, Connections](Connect.Connections.empty)
  implicit val transportLayerEff            = tle
  implicit val cliqueOracleEffect           = SafetyOracle.cliqueOracle[F]
  implicit val lastFinalizedBlockCalculator = LastFinalizedBlockCalculator[F](0f)
  implicit val estimator                    = Estimator[F](maxNumberOfParents, maxParentDepth)
  implicit val synchronyConstraintChecker =
    SynchronyConstraintChecker[F](synchronyConstraintThreshold)
  implicit val lastFinalizedHeightConstraintChecker =
    LastFinalizedHeightConstraintChecker[F](Long.MaxValue)
  implicit val rpConfAsk = createRPConfAsk[F](local)
  implicit val eventBus  = EventPublisher.noop[F]

  // Scalatest `assert` macro needs some member of the Assertions trait.
  // An (inferior) alternative would be to inherit the trait...
  private val scalatestAssertions = new Assertions {}
  import scalatestAssertions._

  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)
  val apiMaxBlocksLimit              = 50

  val validatorId: Option[ValidatorIdentity] =
    if (isReadOnly) none[ValidatorIdentity]
    else Some(ValidatorIdentity(Secp256k1.toPublic(sk), sk, "secp256k1"))

  val approvedBlock =
    ApprovedBlock(
      candidate = (ApprovedBlockCandidate(block = genesis, requiredSigs = 0)),
      sigs = List.empty
    )

  implicit val labF        = LastApprovedBlock.unsafe[F](Some(approvedBlock))
  val postGenesisStateHash = ProtoUtil.postStateHash(genesis)

  implicit val requestedBlocks: RequestedBlocks[F] =
    Ref.unsafe[F, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])

  implicit val commUtil: CommUtil[F]             = CommUtil.of[F]
  implicit val blockRetriever: BlockRetriever[F] = BlockRetriever.of[F]

  val blockProcessingState =
    Ref.unsafe[F, BlockProcessingState](BlockProcessingState(Set.empty, Set.empty))

  implicit val casperEff = new MultiParentCasperImpl[F](
    validatorId,
    genesis,
    shardId,
    finalizationRate,
    blockProcessingLock,
    blockProcessingState
  )

  implicit val rspaceMan                 = RSpaceStateManagerTestImpl()
  val engine                             = new Running(casperEff, approvedBlock, validatorId, ().pure[F], true)
  implicit val engineCell: EngineCell[F] = Cell.unsafe[F, Engine[F]](engine)
  implicit val packetHandlerEff          = CasperPacketHandler[F]

  def addBlock(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    addBlockStatus(ValidBlock.Valid.asRight)(deployDatums: _*)

  def publishBlock(deployDatums: Signed[DeployData]*)(nodes: TestNode[F]*): F[BlockMessage] =
    for {
      block <- addBlock(deployDatums: _*)
      _     <- nodes.toList.filter(_ != this).traverse_(_.receive())
    } yield block

  def propagateBlock(deployDatums: Signed[DeployData]*)(nodes: TestNode[F]*): F[BlockMessage] =
    for {
      block <- addBlock(deployDatums: _*)
      _     <- TestNode.propagate(nodes :+ this)
    } yield block

  def addBlockStatus(
      expectedStatus: ValidBlockProcessing
  )(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    for {
      block  <- createBlock(deployDatums: _*)
      status <- casperEff.addBlock(block)
      _      = assert(status == expectedStatus)
    } yield block

  def createBlock(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    for {
      _                 <- deployDatums.toList.traverse(casperEff.deploy)
      createBlockResult <- casperEff.createBlock
      Created(block)    = createBlockResult
    } yield block

  def receive(): F[Unit] = tls.handleReceive(p => handle[F](p), kp(().pure[F])).void

  val maxSyncAttempts = 10
  def syncWith(nodes: Seq[TestNode[F]]): F[Unit] = {
    val networkMap = nodes.filterNot(_.local == local).map(node => node.local -> node).toMap
    val asked = casper.engine.BlockRetriever
      .RequestedBlocks[F]
      .get
      .map(
        _.values
          .flatMap(
            requested =>
              if (requested.peers.isEmpty)
                /* this means request for blok has been sent to "ether", check everyone */ networkMap.keySet
              else requested.peers
          )
          .toList
      )
    val allSynced = RequestedBlocks[F].get.map(!_.exists(_._2.received == false))
    val doNothing = concurrentF.unit

    def drainQueueOf(peerNode: PeerNode) = networkMap.get(peerNode).fold(doNothing)(_.receive())

    val step = asked.flatMap(_.traverse(drainQueueOf)) >> receive()

    def loop(cnt: Int, done: Boolean) =
      concurrentF.iterateUntilM(cnt -> done)({
        case (i, _) => step.as(i + 1) mproduct (_ => allSynced)
      }) {
        case (i, done) => i >= maxSyncAttempts || done
      }

    (receive() >> allSynced >>= (done => loop(0, done))).flatTap {
      case (_, false) =>
        RequestedBlocks[F].get
          .flatMap(
            requestedBlocks =>
              logEff.warn(
                s"Node $local Still pending requests for blocks (after $maxSyncAttempts attempts): ${requestedBlocks
                  .map { case (hash, req) => PrettyPrinter.buildString(hash) -> req }}"
              )
          )
      case (i, _) =>
        logEff.info(
          s"Node $local has exchanged all the requested blocks with ${networkMap.keysIterator
            .mkString("[", "; ", "]")} after $i round(s)"
        )
    }.void
  }

  def syncWith(node: TestNode[F]): F[Unit]                      = syncWith(IndexedSeq(node))
  def syncWith(node1: TestNode[F], node2: TestNode[F]): F[Unit] = syncWith(IndexedSeq(node1, node2))
  def syncWith(node1: TestNode[F], node2: TestNode[F], rest: TestNode[F]*): F[Unit] =
    syncWith(IndexedSeq(node1, node2) ++ rest)

  def contains(blockHash: BlockHash) = casperEff.contains(blockHash)
  def knowsAbout(blockHash: BlockHash) =
    (contains(blockHash), RequestedBlocks.contains[F](blockHash)).mapN(_ || _)

  def shutoff() = transportLayerEff.clear(local)

  def visualizeDag(startBlockNumber: Int): F[String] = {

    type G[A] = State[StringBuffer, A]
    import cats.mtl.implicits._

    implicit val serializer = new StringSerializer[G]
    val serialize: G[Graphz[G]] => String =
      _.runS(new StringBuffer("")).value.toString

    val result: F[Either[String, String]] = BlockAPI.visualizeDag[F, G, String](
      Int.MaxValue,
      apiMaxBlocksLimit,
      startBlockNumber,
      (ts, lfb) =>
        GraphzGenerator.dagAsCluster[F, G](
          ts,
          lfb,
          GraphConfig(showJustificationLines = true)
        ),
      serialize
    )
    result.map(_.right.get)
  }

  /**
    * Prints a uri on stdout that, when clicked, visualizes the current dag state using ttps://dreampuf.github.io.
    * The dag's shape is passed as Graphviz's dot source code, encoded in the URI's hash.
    */
  def printVisualizeDagUrl(startBlockNumber: Int): F[Unit] =
    for {
      dot <- visualizeDag(startBlockNumber)
      // Java's URLEncode encodes ' ' as '+' instead of '%20', see https://stackoverflow.com/a/5330239/52142
      urlEncoded = URLEncoder.encode(dot, "UTF-8").replaceAll("\\+", "%20")
      _ <- Sync[F].delay {
            println(s"DAG @ $name: https://dreampuf.github.io/GraphvizOnline/#" + urlEncoded)
          }
    } yield ()
}

object TestNode {
  type Effect[A] = Task[A]

  def standaloneEff(
      genesis: GenesisContext,
      storageSize: Long = 1024L * 1024 * 10
  )(
      implicit scheduler: Scheduler
  ): Resource[Effect, TestNode[Effect]] =
    networkEff(
      genesis,
      networkSize = 1,
      storageSize = storageSize
    ).map(_.head)

  def networkEff(
      genesis: GenesisContext,
      networkSize: Int,
      storageSize: Long = 1024L * 1024 * 10,
      synchronyConstraintThreshold: Double = 0d,
      maxNumberOfParents: Int = Estimator.UnlimitedParents,
      maxParentDepth: Option[Int] = None,
      withReadOnlySize: Int = 0
  )(implicit scheduler: Scheduler): Resource[Effect, IndexedSeq[TestNode[Effect]]] =
    networkF[Effect](
      genesis.validatorSks.take(networkSize + withReadOnlySize).toVector,
      genesis.genesisBlock,
      genesis.storageDirectory,
      Resources.mkRuntimeManagerWithHistoryAt[Effect](_)(storageSize),
      synchronyConstraintThreshold,
      maxNumberOfParents,
      maxParentDepth,
      withReadOnlySize
    )(
      Concurrent[Effect],
      TestNetwork.empty[Effect]
    )

  private def networkF[F[_]: Concurrent: TestNetwork](
      sks: IndexedSeq[PrivateKey],
      genesis: BlockMessage,
      storageMatrixPath: Path,
      createRuntime: Path => Resource[F, (RuntimeManager[F], RhoHistoryRepository[F])],
      synchronyConstraintThreshold: Double,
      maxNumberOfParents: Int,
      maxParentDepth: Option[Int],
      withReadOnlySize: Int
  ): Resource[F, IndexedSeq[TestNode[F]]] = {
    val n          = sks.length
    val names      = (1 to n).map(i => if (i <= (n - withReadOnlySize)) s"node-$i" else s"readOnly-$i")
    val isReadOnly = (1 to n).map(i => if (i <= (n - withReadOnlySize)) false else true)
    val peers      = names.map(peerNode(_, 40400))

    val logicalTime: LogicalTime[F] = new LogicalTime[F]

    val nodesF =
      names
        .zip(peers)
        .zip(sks)
        .zip(isReadOnly)
        .toList
        .traverse {
          case (((name, currentPeerNode), sk), isReadOnly) =>
            createNode(
              name,
              currentPeerNode,
              genesis,
              sk,
              storageMatrixPath,
              logicalTime,
              createRuntime,
              synchronyConstraintThreshold,
              maxNumberOfParents,
              maxParentDepth,
              isReadOnly
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
      createRuntime: Path => Resource[F, (RuntimeManager[F], RhoHistoryRepository[F])],
      synchronyConstraintThreshold: Double,
      maxNumberOfParents: Int,
      maxParentDepth: Option[Int],
      isReadOnly: Boolean
  ): Resource[F, TestNode[F]] = {
    val tle                = new TransportLayerTestImpl[F]()
    val tls                = new TransportLayerServerTestImpl[F](currentPeerNode)
    implicit val log       = Log.log[F]
    implicit val metricEff = new Metrics.MetricsNOP[F]
    implicit val spanEff   = NoopSpan[F]()
    for {
      paths <- Resources.copyStorage[F](storageMatrixPath)

      blockStore          <- Resources.mkBlockStoreAt[F](paths.blockStoreDir)
      blockDagStorage     <- Resources.mkBlockDagStorageAt[F](paths.blockDagDir)
      deployStorage       <- Resources.mkDeployStorageAt[F](paths.deployStorageDir)
      casperBufferStorage <- Resources.mkCasperBuferStorate[F](paths.deployStorageDir)
      runtimeManager      <- createRuntime(paths.rspaceDir)

      node <- Resource.liftF(
               for {
                 lastFinalizedStorage <- LastFinalizedFileStorage.make[F](paths.lastFinalizedFile)
                 _                    <- TestNetwork.addPeer(currentPeerNode)
                 blockProcessingLock  <- Semaphore[F](1)
                 node = new TestNode[F](
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
                   synchronyConstraintThreshold,
                   maxNumberOfParents,
                   maxParentDepth,
                   isReadOnly = isReadOnly
                 )(
                   Concurrent[F],
                   blockStore,
                   blockDagStorage,
                   lastFinalizedStorage,
                   deployStorage,
                   metricEff,
                   spanEff,
                   casperBufferStorage,
                   runtimeManager._1,
                   runtimeManager._2
                 )
               } yield node
             )
    } yield node
  }

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  def propagate[F[_]: Monad](nodes: Seq[TestNode[F]]): F[Unit] = {
    val nodesList = nodes.toList
    val peers     = nodesList.map(_.local).toSet
    val network   = nodesList.head.transportLayerEff.testNetworkF
    val heatDeath =
      network.inspect(_.filterKeys(peers.contains(_)).valuesIterator.forall(_.isEmpty))
    val propagation = nodesList.traverse_(_.receive())

    propagation.untilM_(heatDeath)
  }

  def propagate[F[_]: Monad](node1: TestNode[F], node2: TestNode[F]): F[Unit] =
    propagate(Seq(node1, node2))

}
