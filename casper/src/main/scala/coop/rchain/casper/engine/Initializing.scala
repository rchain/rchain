package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.ValidBlock.Valid
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.{RSpaceImporter, RSpaceStateManager}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Event => NodeEvent, _}
import fs2.concurrent.Queue

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

/**
  * initializing engine makes sure node receives Approved State and transitions to Running after
  * */
// format: off
class Initializing[F[_]
  /* Execution */   : Concurrent: Time: Timer
  /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
  /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
  /* Rholang */     : RuntimeManager
  /* Casper */      : LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
  /* Storage */     : BlockStore: ApprovedStore: BlockDagStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
  /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
(
    blockProcessingQueue: Queue[F, BlockMessage],
    blocksInProcessing: Ref[F, Set[BlockHash]],
    casperShardConf: CasperShardConf,
    validatorId: Option[ValidatorIdentity],
    theInit: F[Unit],
    blockMessageQueue: Queue[F, BlockMessage],
    tupleSpaceQueue: Queue[F, StoreItemsMessage],
    trimState: Boolean = true,
    disableStateExporter: Boolean
) extends Engine[F] {

  import Engine._

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlock(peer, ab, disableStateExporter)
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case na: NoApprovedBlockAvailable =>
      logNoApprovedBlockAvailable[F](na.nodeIdentifer) >>
        Time[F].sleep(10.seconds) >>
        CommUtil[F].requestApprovedBlock(trimState)

    case s: StoreItemsMessage =>
      Log[F].info(s"Received ${s.pretty} from $peer.") *> tupleSpaceQueue.enqueue1(s)

    case b: BlockMessage =>
      Log[F]
        .info(s"BlockMessage received ${PrettyPrinter.buildString(b, short = true)} from $peer.") *>
        blockMessageQueue.enqueue1(b)

    case _ => ().pure
  }

  // TEMP: flag for single call for process approved block
  val startRequester = Ref.unsafe(true)

  private def onApprovedBlock(
      sender: PeerNode,
      approvedBlock: ApprovedBlock,
      disableStateExporter: Boolean
  ): F[Unit] = {
    val senderIsBootstrap = RPConfAsk[F].ask.map(_.bootstrap.exists(_ == sender))
    val receivedShard     = approvedBlock.candidate.block.shardId
    val expectedShard     = casperShardConf.shardName
    val shardNameIsValid  = receivedShard == expectedShard

    def handleApprovedBlock = {
      val block = approvedBlock.candidate.block
      for {
        _ <- Log[F].info(
              s"Valid approved block ${PrettyPrinter.buildString(block, short = true)} received. Restoring approved state."
            )

        // Record approved block in DAG
        _ <- BlockDagStorage[F].insert(block, invalid = false, approved = true)

        // Download approved state and all related blocks
        _ <- requestApprovedState(approvedBlock)

        // Approved block is saved after the whole state is received,
        //  to restart requesting if interrupted with incomplete state.
        _ <- ApprovedStore[F].putApprovedBlock(approvedBlock)
        _ <- LastApprovedBlock[F].set(approvedBlock)

        _ <- EventLog[F].publish(
              NodeEvent.ApprovedBlockReceived(
                PrettyPrinter
                  .buildStringNoLimit(block.blockHash)
              )
            )

        _ <- Log[F].info(
              s"Approved state for block ${PrettyPrinter.buildString(block, short = true)} is successfully restored."
            )
      } yield ()
    }

    for {
      // TODO resolve validation of approved block - we should be sure that bootstrap is not lying
      // Might be Validate.approvedBlock is enough but have to check
      isValid <- senderIsBootstrap &&^ shardNameIsValid.pure &&^
                  Validate.approvedBlock[F](approvedBlock)

      _ <- Log[F].info("Received approved block from bootstrap node.").whenA(isValid)

      _ <- Log[F].info("Invalid LastFinalizedBlock received; refusing to add.").whenA(!isValid)

      _ <- Log[F]
            .info(
              s"Connected to the wrong shard. Approved block received from bootstrap is in shard " +
                s"'${receivedShard}' but expected is '${expectedShard}'. Check configuration option shard-name."
            )
            .whenA(!shardNameIsValid)

      // Start only once, when state is true and approved block is valid
      start <- startRequester.modify {
                case true if isValid  => (false, true)
                case true if !isValid => (true, false)
                case _                => (false, false)
              }

      _ <- handleApprovedBlock.whenA(start)
    } yield ()
  }

  def requestApprovedState(approvedBlock: ApprovedBlock): F[Unit] = {
    // Starting minimum block height. When latest blocks are downloaded new minimum will be calculated.
    val block            = approvedBlock.candidate.block
    val startBlockNumber = ProtoUtil.blockNumber(block)
    val minBlockNumberForDeployLifespan =
      Math.max(0, startBlockNumber - MultiParentCasper.deployLifespan)

    for {
      // Request all blocks for Last Finalized State
      blockRequestStream <- LfsBlockRequester.stream(
                             approvedBlock,
                             blockMessageQueue,
                             minBlockNumberForDeployLifespan,
                             hash => CommUtil[F].broadcastRequestForBlock(hash, 1.some),
                             requestTimeout = 30.seconds,
                             BlockStore[F].contains(_),
                             BlockStore[F].getUnsafe,
                             BlockStore[F].put(_, _),
                             Validate.blockHash[F]
                           )

      // Request tuple space state for Last Finalized State
      stateValidator = RSpaceImporter.validateStateItems[F] _
      tupleSpaceStream <- LfsTupleSpaceRequester.stream(
                           approvedBlock,
                           tupleSpaceQueue,
                           (statePartPath, pageSize) =>
                             TransportLayer[F].sendToBootstrap(
                               StoreItemsMessageRequest(statePartPath, 0, pageSize).toProto
                             ),
                           requestTimeout = 2.minutes,
                           RSpaceStateManager[F].importer,
                           stateValidator
                         )

      tupleSpaceLogStream = tupleSpaceStream ++
        fs2.Stream.eval(Log[F].info(s"Rholang state received and saved to store.")).drain

      // Receive the blocks and after populate the DAG
      blockRequestAddDagStream = blockRequestStream.last.unNoneTerminate.evalMap { st =>
        populateDag(approvedBlock.candidate.block, st.lowerBound, st.heightMap)
      }

      // Run both streams in parallel until tuple space and all needed blocks are received
      _ <- fs2.Stream(blockRequestAddDagStream, tupleSpaceLogStream).parJoinUnbounded.compile.drain

      // Transition to Running state
      _ <- createCasperAndTransitionToRunning(approvedBlock)
    } yield ()
  }

  private def populateDag(
      startBlock: BlockMessage,
      minHeight: Long,
      heightMap: SortedMap[Long, Set[BlockHash]]
  ): F[Unit] = {
    import cats.instances.list._

    def addBlockToDag(block: BlockMessage, isInvalid: Boolean): F[Unit] =
      Log[F].info(
        s"Adding ${PrettyPrinter.buildString(block, short = true)}, invalid = $isInvalid."
      ) <* BlockDagStorage[F].insert(block, invalid = isInvalid)

    for {
      _ <- Log[F].info(s"Adding blocks for approved state to DAG.")

      // Latest messages from slashed validators / invalid blocks
      slashedValidators = startBlock.body.state.bonds.filter(_.stake == 0L).map(_.validator)
      invalidBlocks = startBlock.justifications
        .filter(v => slashedValidators.contains(v.validator))
        .map(_.latestBlockHash)
        .toSet

      // Add sorted DAG in order from approved block to oldest
      _ <- heightMap.flatMap(_._2).toList.reverse.traverse_ { hash =>
            for {
              block <- BlockStore[F].getUnsafe(hash)
              // If sender has stake 0 in approved block, this means that sender has been slashed and block is invalid
              isInvalid = invalidBlocks(block.blockHash)
              // Filter older not necessary blocks
              blockHeight   = ProtoUtil.blockNumber(block)
              blockHeightOk = blockHeight >= minHeight
              // Add block to DAG
              _ <- addBlockToDag(block, isInvalid).whenA(blockHeightOk)
            } yield ()
          }

      _ <- Log[F].info(s"Blocks for approved state added to DAG.")
    } yield ()
  }

  private def createCasperAndTransitionToRunning(approvedBlock: ApprovedBlock): F[Unit] =
    for {
      _ <- Log[F].info("MultiParentCasper instance created.")
      _ <- transitionToRunning[F](
            blockProcessingQueue,
            blocksInProcessing,
            approvedBlock,
            validatorId,
            ().pure,
            disableStateExporter
          )
      _ <- CommUtil[F].sendForkChoiceTipRequest
    } yield ()
}
