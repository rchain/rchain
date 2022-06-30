package coop.rchain.casper.helper

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ContextShift, Resource, Sync, Timer}
import cats.syntax.all._
import cats.{Monad, Parallel}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper
import coop.rchain.casper._
import coop.rchain.casper.blocks.BlockProcessor
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.engine.BlockRetriever._
import coop.rchain.casper.engine._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.{Resources, RuntimeManager}
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.casper.util.comm.TestNetwork.TestNetwork
import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.comm.transport.CommunicationResponse
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockVersion
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rspace.syntax._
import coop.rchain.shared._
import fs2.concurrent.Queue
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertions

import java.nio.file.Path
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

case class TestNode[F[_]: Sync: Timer](
    name: String,
    local: PeerNode,
    tle: TransportLayerTestImpl[F],
    tls: TransportLayerServerTestImpl[F],
    genesis: BlockMessage,
    validatorIdOpt: Option[ValidatorIdentity],
    logicalTime: LogicalTime[F],
    synchronyConstraintThreshold: Double,
    dataDir: Path,
    maxNumberOfParents: Int = Int.MaxValue,
    maxParentDepth: Option[Int] = Int.MaxValue.some,
    isReadOnly: Boolean = false,
    triggerProposeFOpt: Option[ProposeFunction[F]],
    processBlock: BlockMessage => F[ValidBlockProcessing],
    blockStoreEffect: BlockStore[F],
    approvedStoreEffect: ApprovedStore[F],
    blockDagStorageEffect: BlockDagStorage[F],
    commUtilEffect: CommUtil[F],
    blockRetrieverEffect: BlockRetriever[F],
    metricEffect: Metrics[F],
    spanEffect: Span[F],
    runtimeManagerEffect: RuntimeManager[F],
    rhoHistoryRepositoryEffect: RhoHistoryRepository[F],
    logEffect: LogStub[F],
    requestedBlocksEffect: RequestedBlocks[F],
    timeEffect: Time[F],
    transportLayerEffect: TransportLayerTestImpl[F],
    connectionsCellEffect: Ref[F, Connections],
    rpConfAskEffect: RPConfAsk[F],
    eventPublisherEffect: EventPublisher[F],
    routingMessageQueue: Queue[F, RoutingMessage],
    shardName: String,
    minPhloPrice: Long
)(implicit concurrentF: Concurrent[F]) {
  // Scalatest `assert` macro needs some member of the Assertions trait.
  // An (inferior) alternative would be to inherit the trait...
  private val scalatestAssertions = new Assertions {}
  import scalatestAssertions._

  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)
  val apiMaxBlocksLimit              = 50

  implicit val requestedBlocks: RequestedBlocks[F]           = requestedBlocksEffect
  implicit val logEff: LogStub[F]                            = logEffect
  implicit val blockStore: BlockStore[F]                     = blockStoreEffect
  implicit val approvedStore: ApprovedStore[F]               = approvedStoreEffect
  implicit val blockDagStorage: BlockDagStorage[F]           = blockDagStorageEffect
  implicit val cu: CommUtil[F]                               = commUtilEffect
  implicit val br: BlockRetriever[F]                         = blockRetrieverEffect
  implicit val me: Metrics[F]                                = metricEffect
  implicit val sp: Span[F]                                   = spanEffect
  implicit val runtimeManager: RuntimeManager[F]             = runtimeManagerEffect
  implicit val rhoHistoryRepository: RhoHistoryRepository[F] = rhoHistoryRepositoryEffect
  implicit val t: Time[F]                                    = timeEffect
  implicit val transportLayerEff: TransportLayerTestImpl[F]  = transportLayerEffect
  implicit val connectionsCell: Ref[F, Connections]          = connectionsCellEffect
  implicit val rp: RPConfAsk[F]                              = rpConfAskEffect
  implicit val ep: EventPublisher[F]                         = eventPublisherEffect

  val finalizedFringe = FinalizedFringe(Seq(genesis.blockHash), genesis.postStateHash)

  val postGenesisStateHash = genesis.postStateHash

  implicit val rspaceMan = RSpaceStateManagerTestImpl()

  def proposeSync: F[BlockHash] =
    for {
      v <- triggerProposeFOpt match {
            case Some(p) => p(false)
            case None =>
              Sync[F]
                .raiseError(new Exception("Propose is called in read-only mode."))
                .as(ProposerEmpty)
          }
      r <- v match {
            case ProposerSuccess(_, b) => b.blockHash.pure[F]
            case _ =>
              Sync[F]
                .raiseError(new Exception("Propose failed or another in progress"))
                .as(ByteString.EMPTY)
          }
    } yield r

  def deploy(dd: Signed[DeployData]) =
    MultiParentCasper.deploy[F](dd)

  def addBlock(block: BlockMessage): F[ValidBlockProcessing] =
    processBlock(block)

  def addBlock(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    addBlockStatus(ValidBlock.Valid.asRight)(deployDatums: _*)

  def publishBlock(deployDatums: Signed[DeployData]*)(nodes: TestNode[F]*): F[BlockMessage] =
    for {
      block <- addBlock(deployDatums: _*)
      _     <- nodes.toList.filter(_ != this).traverse_(_.addBlock(block))
    } yield block

  def propagateBlock(deployDatums: Signed[DeployData]*)(nodes: TestNode[F]*): F[BlockMessage] =
    for {
      _       <- Log[F].debug((s"\n$name creating block"))
      block   <- addBlock(deployDatums: _*)
      targets = nodes diff Seq(this)
      _ <- Log[F].debug(
            s"${name} ! [${PrettyPrinter.buildString(block, true)}] => ${targets.map(_.name).mkString(" ; ")}"
          )
      _ <- targets.toList.traverse_ { node =>
            node.addBlock(block)
          }
    } yield block

  def addBlockStatus(
      expectedStatus: ValidBlockProcessing
  )(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    for {
      r              <- createBlock(deployDatums: _*)
      Created(block) = r
      status         <- addBlock(block)
      _              = assert(status == expectedStatus)
    } yield block

  def createBlock(deployDatums: Signed[DeployData]*): F[BlockCreatorResult] =
    for {
      _ <- deployDatums.toList.traverse(
            deploy =>
              for {
                res <- MultiParentCasper.deploy[F](deploy)
                _   = assert(res.isRight, s"Deploy error ${deploy} with\n ${res.left}")
              } yield ()
          )
      preState <- MultiParentCasper.getPreStateForNewBlock[F]
      createBlockResult <- BlockCreator.create(
                            preState,
                            validatorIdOpt.get,
                            shardName
                          )
    } yield createBlockResult

  // This method assumes that block will be created sucessfully
  def createBlockUnsafe(deployDatums: Signed[DeployData]*): F[BlockMessage] =
    for {
      _        <- deployDatums.toList.traverse(MultiParentCasper.deploy[F])
      preState <- MultiParentCasper.getPreStateForNewBlock[F]
      createBlockResult <- BlockCreator.create(
                            preState,
                            validatorIdOpt.get,
                            shardName
                          )
      block <- createBlockResult match {
                case Created(b) => b.pure[F]
                case err =>
                  concurrentF.raiseError[BlockMessage](
                    new Throwable(s"failed creating block: $err")
                  )
              }
    } yield block

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
                            addBlock(b).as(CommunicationResponse.handledWithoutMessage)
                          case _ => handle[F](p, routingMessageQueue)
                        }
                    )
                }
                case _ => handle[F](p, routingMessageQueue)
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
    val allSynced = RequestedBlocks[F].get.map(_.isEmpty)
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

  def contains(hash: BlockHash) = blockDagStorage.getRepresentation.map(_.contains(hash))

  def knowsAbout(blockHash: BlockHash) =
    (contains(blockHash), RequestedBlocks.contains[F](blockHash)).mapN(_ || _)

  def shutoff() = transportLayerEff.clear(local)

  val lastFinalizedBlock = MultiParentCasper.lastFinalizedBlock
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
      maxNumberOfParents: Int = Int.MaxValue,
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
                  _ => n.connectionsCell.update(_.addConn(m.local))
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
      storageDir: Path,
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
      newStorageDir   <- Resources.copyStorage[F](storageDir)
      kvm             <- Resource.eval(Resources.mkTestRNodeStoreManager(newStorageDir))
      blockStore      <- Resource.eval(BlockStore(kvm))
      approvedStore   <- Resource.eval(approvedStore.create(kvm))
      blockDagStorage <- Resource.eval(BlockDagKeyValueStorage.create(kvm))
      rSpaceStore     <- Resource.eval(kvm.rSpaceStores)
      mStore          <- Resource.eval(RuntimeManager.mergeableStore(kvm))
      runtimeManager <- Resource.eval(
                         RuntimeManager(
                           rSpaceStore,
                           mStore,
                           BlockRandomSeed.nonNegativeMergeableTagName(genesis.shardId),
                           RuntimeManager.noOpExecutionTracker[F]
                         )
                       )

      // Shard configuration
      shardName    = genesis.shardId
      minPhloPrice = 1L

      node <- Resource.eval({
               implicit val bs                    = blockStore
               implicit val as                    = approvedStore
               implicit val bds                   = blockDagStorage
               implicit val rm                    = runtimeManager
               implicit val rhr                   = runtimeManager.getHistoryRepo
               implicit val logEff                = new LogStub[F](Log.log[F])
               implicit val timeEff               = logicalTime
               implicit val connectionsCell       = Ref.unsafe[F, Connections](Connect.Connections.empty)
               implicit val transportLayerEff     = tle
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
                   case Some(vi) =>
                     Proposer[F](vi, shardName, minPhloPrice).some
                   case None => None
                 }
                 // propose function in casper tests is always synchronous
                 triggerProposeFOpt = proposer.map(
                   p =>
                     (_: Boolean) =>
                       for {
                         d <- Deferred[F, ProposerResult]
                         r <- p.propose(false, d)
                         r <- d.get
                       } yield r
                 )

                 // Validate block and add it to the DAG
                 // - sync version without block receiver and block processor async processing
                 saveAndValidateBlock = (block: BlockMessage) => {
                   import coop.rchain.blockstorage.syntax._

                   for {
                     _      <- BlockStore[F].put(block)
                     result <- BlockProcessor.validateAndAddToDag(block, shardName, minPhloPrice)
                   } yield result
                 }

                 // Remove TransportLayer handling in TestNode (too low level for these tests)
                 routingMessageQueue <- Queue.unbounded[F, RoutingMessage]

                 node = new TestNode[F](
                   name,
                   currentPeerNode,
                   tle,
                   tls,
                   genesis,
                   validatorId,
                   logicalTime,
                   synchronyConstraintThreshold,
                   newStorageDir,
                   maxNumberOfParents,
                   maxParentDepth,
                   isReadOnly = isReadOnly,
                   triggerProposeFOpt = triggerProposeFOpt,
                   processBlock = saveAndValidateBlock,
                   blockStoreEffect = bs,
                   approvedStoreEffect = as,
                   blockDagStorageEffect = bds,
                   runtimeManagerEffect = rm,
                   rhoHistoryRepositoryEffect = rhr,
                   spanEffect = spanEff,
                   logEffect = logEff,
                   timeEffect = timeEff,
                   connectionsCellEffect = connectionsCell,
                   transportLayerEffect = transportLayerEff,
                   rpConfAskEffect = rpConfAsk,
                   eventPublisherEffect = eventBus,
                   commUtilEffect = commUtil,
                   requestedBlocksEffect = requestedBlocks,
                   blockRetrieverEffect = blockRetriever,
                   metricEffect = metricEff,
                   routingMessageQueue = routingMessageQueue,
                   shardName = shardName,
                   minPhloPrice = minPhloPrice
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
