package coop.rchain.casper.helper

import java.net.URLEncoder
import java.nio.file.Path
import cats.{Monad, Parallel}
import cats.data.State
import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import cats.effect.{Concurrent, ContextShift, Resource, Sync, Timer}
import cats.implicits._
import cats.syntax.all.none
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.rspace.syntax._
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.{
  BlockDagKeyValueStorage,
  BlockDagRepresentation,
  BlockDagStorage
}
import coop.rchain.casper.util.rholang.Resources.mkTestRNodeStoreManager
import coop.rchain.blockstorage.deploy.LMDBDeployStorage.Config
import coop.rchain.blockstorage.deploy.{DeployStorage, LMDBDeployStorage}
import coop.rchain.casper
import coop.rchain.casper.api.{BlockAPI, GraphConfig, GraphzGenerator}
import coop.rchain.casper.blocks.BlockProcessor
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.engine.BlockRetriever._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.TestNetwork.TestNetwork
import coop.rchain.casper.util.comm.{CasperPacketHandler, _}
import coop.rchain.casper.util.rholang.Resources.StoragePaths
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.casper.{Casper, ValidBlock, _}
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.comm.transport.CommunicationResponse
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.graphz.{Graphz, StringSerializer}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.Resources.mkTempDir
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.shared._
import fs2.{Pipe, Stream}
import fs2.concurrent.Queue
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertions

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

case class TestNode[F[_]: Timer](
    name: String,
    val local: PeerNode,
    tle: TransportLayerTestImpl[F],
    tls: TransportLayerServerTestImpl[F],
    val genesis: BlockMessage,
    validatorIdOpt: Option[ValidatorIdentity],
    logicalTime: LogicalTime[F],
    synchronyConstraintThreshold: Double,
    val dataPath: StoragePaths,
    maxNumberOfParents: Int = Estimator.UnlimitedParents,
    maxParentDepth: Option[Int] = Int.MaxValue.some,
    shardId: String = "root",
    finalizationRate: Int = 1,
    isReadOnly: Boolean = false,
    triggerProposeFOpt: Option[ProposeFunction[F]],
    blockProcessorQueue: Queue[F, (Casper[F], BlockMessage)],
    blockProcessorState: Ref[F, Set[BlockHash]],
    blockProcessingPipe: Pipe[
      F,
      (Casper[F], BlockMessage),
      ValidBlockProcessing
    ],
    blockStoreEffect: BlockStore[F],
    blockDagStorageEffect: BlockDagStorage[F],
    deployStorageEffect: DeployStorage[F],
    commUtilEffect: CommUtil[F],
    blockRetrieverEffect: BlockRetriever[F],
    metricEffect: Metrics[F],
    spanEffect: Span[F],
    casperBufferStorageEffect: CasperBufferStorage[F],
    runtimeManagerEffect: RuntimeManager[F],
    rhoHistoryRepositoryEffect: RhoHistoryRepository[F],
    logEffect: LogStub[F],
    requestedBlocksEffect: RequestedBlocks[F],
    syncConstraintCheckerEffect: SynchronyConstraintChecker[F],
    lastFinalizedHeightCheckerEffect: LastFinalizedHeightConstraintChecker[F],
    estimatorEffect: Estimator[F],
    safetyOracleEffect: SafetyOracle[F],
    timeEffect: Time[F],
    transportLayerEffect: TransportLayerTestImpl[F],
    connectionsCellEffect: Cell[F, Connections],
    rpConfAskEffect: RPConfAsk[F],
    eventPublisherEffect: EventPublisher[F]
)(implicit concurrentF: Concurrent[F]) {
  // Scalatest `assert` macro needs some member of the Assertions trait.
  // An (inferior) alternative would be to inherit the trait...
  private val scalatestAssertions = new Assertions {}
  import scalatestAssertions._

  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)
  val apiMaxBlocksLimit              = 50

  implicit val requestedBlocks: RequestedBlocks[F]            = requestedBlocksEffect
  implicit val validatorId: Option[ValidatorIdentity]         = validatorIdOpt
  implicit val logEff: LogStub[F]                             = logEffect
  implicit val cliqueOracleEffect: SafetyOracle[F]            = safetyOracleEffect
  implicit val blockStore: BlockStore[F]                      = blockStoreEffect
  implicit val blockDagStorage: BlockDagStorage[F]            = blockDagStorageEffect
  implicit val ds: DeployStorage[F]                           = deployStorageEffect
  implicit val cu: CommUtil[F]                                = commUtilEffect
  implicit val br: BlockRetriever[F]                          = blockRetrieverEffect
  implicit val m: Metrics[F]                                  = metricEffect
  implicit val s: Span[F]                                     = spanEffect
  implicit val cbs: CasperBufferStorage[F]                    = casperBufferStorageEffect
  implicit val runtimeManager: RuntimeManager[F]              = runtimeManagerEffect
  implicit val rhoHistoryRepository: RhoHistoryRepository[F]  = rhoHistoryRepositoryEffect
  implicit val scch: SynchronyConstraintChecker[F]            = syncConstraintCheckerEffect
  implicit val lfhch: LastFinalizedHeightConstraintChecker[F] = lastFinalizedHeightCheckerEffect
  implicit val e: Estimator[F]                                = estimatorEffect
  implicit val t: Time[F]                                     = timeEffect
  implicit val transportLayerEff: TransportLayerTestImpl[F]   = transportLayerEffect
  implicit val connectionsCell: Cell[F, Connections]          = connectionsCellEffect
  implicit val rp: RPConfAsk[F]                               = rpConfAskEffect
  implicit val ep: EventPublisher[F]                          = eventPublisherEffect

  val approvedBlock =
    ApprovedBlock(
      candidate = (ApprovedBlockCandidate(block = genesis, requiredSigs = 0)),
      sigs = List.empty
    )

  implicit val labF        = LastApprovedBlock.unsafe[F](Some(approvedBlock))
  val postGenesisStateHash = ProtoUtil.postStateHash(genesis)

  val shardConf = CasperShardConf(
    faultToleranceThreshold = 0,
    shardName = shardId,
    parentShardId = "",
    finalizationRate = finalizationRate,
    maxNumberOfParents = maxNumberOfParents,
    maxParentDepth = maxParentDepth.getOrElse(Int.MaxValue),
    synchronyConstraintThreshold = synchronyConstraintThreshold.toFloat,
    heightConstraintThreshold = Long.MaxValue,
    // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
    // Required to enable protection from re-submitting duplicate deploys
    deployLifespan = 50,
    casperVersion = 1,
    configVersion = 1,
    bondMinimum = 0,
    bondMaximum = Long.MaxValue,
    epochLength = 10000,
    quarantineLength = 20000
  )

  implicit val casperEff = new MultiParentCasperImpl[F](
    validatorId,
    shardConf,
    genesis
  )

  implicit val rspaceMan = RSpaceStateManagerTestImpl()
  val engine =
    new coop.rchain.casper.engine.Running(
      blockProcessorQueue,
      blockProcessorState,
      casperEff,
      approvedBlock,
      validatorId,
      ().pure[F],
      true
    )
  implicit val engineCell: EngineCell[F] = Cell.unsafe[F, Engine[F]](engine)
  implicit val packetHandlerEff          = CasperPacketHandler[F]

  def proposeSync: F[BlockHash] =
    for {
      v <- (triggerProposeFOpt match {
            case Some(p) => p(casperEff, false)
            case None =>
              Sync[F]
                .raiseError(new Exception("Propose is called in read-only mode."))
                .as(ProposerEmpty)
          })
      r <- v match {
            case ProposerSuccess(_, b) => b.blockHash.pure[F]
            case _ =>
              Sync[F]
                .raiseError(new Exception("Propose failed or another in progress"))
                .as(ByteString.EMPTY)
          }
    } yield r

  def addBlock(block: BlockMessage): F[ValidBlockProcessing] =
    Stream((casperEff, block)).through(blockProcessingPipe).compile.lastOrError

  def addBlock(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    addBlockStatus(ValidBlock.Valid.asRight)(deployDatums: _*)

  def publishBlock(deployDatums: Signed[DeployData]*)(nodes: TestNode[F]*): F[BlockMessage] =
    for {
      block <- addBlock(deployDatums: _*)
      _     <- nodes.toList.filter(_ != this).traverse_(_.handleReceive())
    } yield block

  def propagateBlock(deployDatums: Signed[DeployData]*)(nodes: TestNode[F]*): F[BlockMessage] =
    for {
      _       <- Log[F].debug((s"\n$name creating block"))
      block   <- addBlock(deployDatums: _*)
      targets = nodes diff Seq(this)
      _ <- Log[F].debug(
            s"${name} ! [${PrettyPrinter.buildString(block, true)}] => ${targets.map(_.name).mkString(" ; ")}"
          )
      _ <- (targets).toList.traverse_ { node =>
            node.processBlock(block)
          }
    } yield block

  def addBlockStatus(
      expectedStatus: ValidBlockProcessing
  )(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    for {
      r              <- createBlock(deployDatums: _*)
      Created(block) = r
      status         <- processBlock(block)
      _              = assert(status == expectedStatus)
    } yield block

  def createBlock(deployDatums: Signed[DeployData]*): F[BlockCreatorResult] =
    for {
      _                 <- deployDatums.toList.traverse(casperEff.deploy)
      cs                <- casperEff.getSnapshot
      vid               <- casperEff.getValidator
      createBlockResult <- BlockCreator.create(cs, vid.get)
    } yield createBlockResult

  // This method assumes that block will be created sucessfully
  def createBlockUnsafe(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    for {
      _                 <- deployDatums.toList.traverse(casperEff.deploy)
      cs                <- casperEff.getSnapshot
      vid               <- casperEff.getValidator
      createBlockResult <- BlockCreator.create(cs, vid.get)
      block <- createBlockResult match {
                case Created(b) => b.pure[F]
                case _ =>
                  concurrentF.raiseError[BlockMessage](
                    new Throwable(s"failed creating block: ${e}")
                  )
              }
    } yield block

  def processBlock(b: BlockMessage): F[ValidBlockProcessing] =
    for {
      r <- Stream((casperEff, b)).through(blockProcessingPipe).compile.lastOrError
    } yield r

  def handleReceive(): F[Unit] =
    tls
      .handleReceive(
        (
            p =>
              p.message match {
                case Protocol.Message.Packet(packet) => {
                  toCasperMessageProto(packet).toEither
                    .flatMap(proto => CasperMessage.from(proto))
                    .fold(
                      err =>
                        Log[F]
                          .warn(s"Could not extract casper message from packet")
                          .as(CommunicationResponse.notHandled(UnknownCommError(""))),
                      message =>
                        message match {
                          case b: BlockMessage =>
                            processBlock(b).as(CommunicationResponse.handledWithoutMessage)
                          case _ => handle[F](p)
                        }
                    )
                }
                case _ => handle[F](p)
              }
          ),
        kp(().pure[F])
      )
      .void

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
    val allSynced = RequestedBlocks[F].get.map(b => { !b.exists(_._2.received == false) })
    val doNothing = concurrentF.unit

    def drainQueueOf(peerNode: PeerNode) =
      networkMap.get(peerNode).fold(doNothing)(_.handleReceive())

    val step = asked.flatMap(_.traverse(drainQueueOf)) >> handleReceive()

    def loop(cnt: Int, done: Boolean) =
      concurrentF.iterateUntilM(cnt -> done)({
        case (i, _) => step.as(i + 1) mproduct (_ => allSynced)
      }) {
        case (i, done) => i >= maxSyncAttempts || done
      }

    (handleReceive() >> allSynced >>= (done => loop(0, done))).flatTap {
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

  def standaloneEff(genesis: GenesisContext)(
      implicit scheduler: Scheduler
  ): Resource[Effect, TestNode[Effect]] =
    networkEff(
      genesis,
      networkSize = 1
    ).map(_.head)

  def networkEff(
      genesis: GenesisContext,
      networkSize: Int,
      synchronyConstraintThreshold: Double = 0d,
      maxNumberOfParents: Int = Estimator.UnlimitedParents,
      maxParentDepth: Option[Int] = None,
      withReadOnlySize: Int = 0
  )(implicit scheduler: Scheduler): Resource[Effect, IndexedSeq[TestNode[Effect]]] = {
    implicit val c = Concurrent[Effect]
    implicit val n = TestNetwork.empty[Effect]

    networkF[Effect](
      genesis.validatorSks.take(networkSize + withReadOnlySize).toVector,
      genesis.genesisBlock,
      genesis.storageDirectory,
      synchronyConstraintThreshold,
      maxNumberOfParents,
      maxParentDepth,
      withReadOnlySize
    )
  }

  private def networkF[F[_]: Concurrent: Parallel: ContextShift: Timer: TestNetwork](
      sks: IndexedSeq[PrivateKey],
      genesis: BlockMessage,
      storageMatrixPath: Path,
      synchronyConstraintThreshold: Double,
      maxNumberOfParents: Int,
      maxParentDepth: Option[Int],
      withReadOnlySize: Int
  )(implicit s: Scheduler): Resource[F, IndexedSeq[TestNode[F]]] = {
    val n           = sks.length
    val names       = (1 to n).map(i => if (i <= (n - withReadOnlySize)) s"node-$i" else s"readOnly-$i")
    val isReadOnly  = (1 to n).map(i => if (i <= (n - withReadOnlySize)) false else true)
    val peers       = names.map(peerNode(_, 40400))
    val logicalTime = new LogicalTime[F]

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
              synchronyConstraintThreshold,
              maxNumberOfParents,
              maxParentDepth,
              isReadOnly
            )
        }
        .map(_.toVector)

    nodesF.evalMap { nodes =>
      import Connections._
      //make sure all nodes know about each other
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
    }
  }

  private def createNode[F[_]: Concurrent: Timer: Parallel: ContextShift: TestNetwork](
      name: String,
      currentPeerNode: PeerNode,
      genesis: BlockMessage,
      sk: PrivateKey,
      storageMatrixPath: Path,
      logicalTime: LogicalTime[F],
      synchronyConstraintThreshold: Double,
      maxNumberOfParents: Int,
      maxParentDepth: Option[Int],
      isReadOnly: Boolean
  )(implicit s: Scheduler): Resource[F, TestNode[F]] = {
    val tle                = new TransportLayerTestImpl[F]()
    val tls                = new TransportLayerServerTestImpl[F](currentPeerNode)
    implicit val log       = Log.log[F]
    implicit val metricEff = new Metrics.MetricsNOP[F]
    implicit val spanEff   = new NoopSpan[F]
    for {
      paths                             <- Resources.copyStorage[F](storageMatrixPath)
      kvm                               <- Resource.liftF(Resources.mkTestRNodeStoreManager(paths.storageDir))
      blockStore                        <- Resource.liftF(KeyValueBlockStore(kvm))
      blockDagStorage                   <- Resource.liftF(BlockDagKeyValueStorage.create(kvm))
      deployStoreConfig                 = Config(paths.deployStorageDir, 1024L * 1024L * 1024L)
      deployStorage                     <- LMDBDeployStorage.make[F](deployStoreConfig)
      casperBufferStorage               <- Resource.liftF(Resources.mkCasperBufferStorage[F](kvm))
      rspaceStore                       <- Resource.liftF(kvm.rSpaceStores)
      runtimes                          <- Resource.liftF(RhoRuntime.createRuntimes(rspaceStore, true, Seq.empty))
      (runtime, replayRuntime, history) = runtimes
      runtimeManager                    <- Resource.liftF(RuntimeManager.fromRuntimes(runtime, replayRuntime, history))

      node <- Resource.liftF({
               implicit val bs                         = blockStore
               implicit val bds                        = blockDagStorage
               implicit val ds                         = deployStorage
               implicit val cbs                        = casperBufferStorage
               implicit val rm                         = runtimeManager
               implicit val rhr                        = history
               implicit val logEff                     = new LogStub[F](Log.log[F])
               implicit val timeEff                    = logicalTime
               implicit val connectionsCell            = Cell.unsafe[F, Connections](Connect.Connections.empty)
               implicit val transportLayerEff          = tle
               implicit val cliqueOracleEffect         = SafetyOracle.cliqueOracle[F]
               implicit val synchronyConstraintChecker = SynchronyConstraintChecker[F]
               implicit val lastFinalizedHeightConstraintChecker =
                 LastFinalizedHeightConstraintChecker[F]
               implicit val estimator             = Estimator[F](maxNumberOfParents, maxParentDepth)
               implicit val rpConfAsk             = createRPConfAsk[F](currentPeerNode)
               implicit val eventBus              = EventPublisher.noop[F]
               implicit val commUtil: CommUtil[F] = CommUtil.of[F]
               implicit val requestedBlocks: RequestedBlocks[F] =
                 Ref.unsafe[F, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
               implicit val blockRetriever: BlockRetriever[F] = BlockRetriever.of[F]

               for {
                 _ <- TestNetwork.addPeer(currentPeerNode)

                 // Proposer
                 validatorId = if (isReadOnly)
                   none[ValidatorIdentity]
                 else
                   Some(ValidatorIdentity(Secp256k1.toPublic(sk), sk, "secp256k1"))

                 proposer = validatorId match {
                   case Some(vi) => Proposer[F](vi).some
                   case None     => None
                 }
                 // propose function in casper tests is always synchronous
                 triggerProposeFOpt = proposer.map(
                   p =>
                     (casper: Casper[F], _: Boolean) =>
                       for {
                         d <- Deferred[F, ProposerResult]
                         r <- p.propose(casper, false, d)
                         r <- d.get
                       } yield r
                 )
                 // Block processor
                 blockProcessor = BlockProcessor[F]

                 blockProcessingPipe = {
                   in: fs2.Stream[F, (Casper[F], BlockMessage)] =>
                     in.evalMap(v => {
                       val (c, b) = v
                       blockProcessor
                         .checkIfOfInterest(c, b)
                         .ifM(
                           blockProcessor
                             .checkIfWellFormedAndStore(b)
                             .ifM(
                               blockProcessor
                                 .checkDependenciesWithEffects(c, b)
                                 .ifM(
                                   blockProcessor.validateWithEffects(c, b),
                                   BlockStatus.missingBlocks.asLeft[ValidBlock].pure[F]
                                 ),
                               BlockStatus.invalidFormat.asLeft[ValidBlock].pure[F]
                             ),
                           BlockStatus.notOfInterest.asLeft[ValidBlock].pure[F]
                         )
                     })
                 }
                 blockProcessorQueue <- Queue.unbounded[F, (Casper[F], BlockMessage)]
                 blockProcessorState <- Ref.of[F, Set[BlockHash]](Set.empty)

                 node = new TestNode[F](
                   name,
                   currentPeerNode,
                   tle,
                   tls,
                   genesis,
                   validatorId,
                   logicalTime,
                   synchronyConstraintThreshold,
                   paths,
                   maxNumberOfParents,
                   maxParentDepth,
                   isReadOnly = isReadOnly,
                   triggerProposeFOpt = triggerProposeFOpt,
                   blockProcessorQueue = blockProcessorQueue,
                   blockProcessorState = blockProcessorState,
                   blockProcessingPipe = blockProcessingPipe,
                   blockStoreEffect = bs,
                   blockDagStorageEffect = bds,
                   deployStorageEffect = ds,
                   casperBufferStorageEffect = cbs,
                   runtimeManagerEffect = rm,
                   rhoHistoryRepositoryEffect = rhr,
                   spanEffect = spanEff,
                   logEffect = logEff,
                   timeEffect = timeEff,
                   connectionsCellEffect = connectionsCell,
                   transportLayerEffect = transportLayerEff,
                   safetyOracleEffect = cliqueOracleEffect,
                   syncConstraintCheckerEffect = synchronyConstraintChecker,
                   lastFinalizedHeightCheckerEffect = lastFinalizedHeightConstraintChecker,
                   estimatorEffect = estimator,
                   rpConfAskEffect = rpConfAsk,
                   eventPublisherEffect = eventBus,
                   commUtilEffect = commUtil,
                   requestedBlocksEffect = requestedBlocks,
                   blockRetrieverEffect = blockRetriever,
                   metricEffect = metricEff
                 )
               } yield node
             })
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
    val propagation = nodesList.traverse_(_.handleReceive())

    propagation.untilM_(heatDeath)
  }

  def propagate[F[_]: Monad](node1: TestNode[F], node2: TestNode[F]): F[Unit] =
    propagate(Seq(node1, node2))

}
