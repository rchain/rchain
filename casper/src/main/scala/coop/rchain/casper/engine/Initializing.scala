package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.ValidBlock.Valid
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rholang.interpreter.storage
import coop.rchain.rspace.state.{RSpaceImporter, RSpaceStateManager}
import coop.rchain.shared
import coop.rchain.shared._
import fs2.concurrent.Queue

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

/**
  * initializing engine makes sure node receives Approved State and transitions to Running after
  * */
// format: off
class Initializing[F[_]
  /* Execution */   : Concurrent: Time
  /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
  /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
  /* Rholang */     : RuntimeManager
  /* Casper */      : Estimator: SafetyOracle: LastFinalizedBlockCalculator: LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
  /* Storage */     : BlockStore: BlockDagStorage: LastFinalizedStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
  /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
(
    shardId: String,
    finalizationRate: Int,
    validatorId: Option[ValidatorIdentity],
    theInit: F[Unit],
    blockMessageQueue: Queue[F, BlockMessage],
    tupleSpaceQueue: Queue[F, StoreItemsMessage],
    trimState: Boolean = false,
    enableStateExporter: Boolean
) extends Engine[F] {

  import Engine._

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlock(peer, ab, enableStateExporter)
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
      enableStateExporter: Boolean
  ): F[Unit] = {
    val senderIsBootstrap = RPConfAsk[F].ask.map(_.bootstrap.exists(_ == sender))

    def handleApprovedBlock = {
      val block = approvedBlock.candidate.block
      for {
        _ <- Log[F].info(
              s"Valid approved block ${PrettyPrinter.buildString(block, short = true)} received. Restoring approved state."
            )

        // Download approved state and all related blocks
        _ <- requestApprovedState(approvedBlock)

        // Approved block is saved after the whole state is received,
        //  to restart requesting if interrupted with incomplete state.
        _ <- BlockStore[F].putApprovedBlock(approvedBlock)
        _ <- LastApprovedBlock[F].set(approvedBlock)

        _ <- EventLog[F].publish(
              shared.Event.ApprovedBlockReceived(
                PrettyPrinter
                  .buildStringNoLimit(block.blockHash)
              )
            )

        // Update last finalized block with received block hash
        _ <- LastFinalizedStorage[F].put(block.blockHash)

        _ <- Log[F].info(
              s"Approved state for block ${PrettyPrinter.buildString(block, short = true)} is successfully restored."
            )
      } yield ()
    }

    for {
      // TODO resolve validation of approved block - we should be sure that bootstrap is not lying
      // Might be Validate.approvedBlock is enough but have to check
      isValid <- senderIsBootstrap &&^ Validate.approvedBlock[F](approvedBlock)

      _ <- Log[F].info("Received approved block from bootstrap node.").whenA(isValid)

      _ <- Log[F].info("Invalid LastFinalizedBlock received; refusing to add.").whenA(!isValid)

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
      Math.max(0, startBlockNumber - MultiParentCasperImpl.deployLifespan)

    for {
      // Request all blocks for Last Finalized State
      blockRequestStream <- LfsBlockRequester.stream(
                             approvedBlock,
                             blockMessageQueue,
                             minBlockNumberForDeployLifespan,
                             hash => CommUtil[F].broadcastRequestForBlock(hash, 1.some),
                             requestTimeout = 30.seconds,
                             BlockStore[F].contains,
                             BlockStore[F].getUnsafe,
                             BlockStore[F].put,
                             validateBlock
                           )

      // Request tuple space state for Last Finalized State
      stateValidator = {
        implicit val codecPar  = storage.serializePar.toSizeHeadCodec
        implicit val codecBind = storage.serializeBindPattern.toSizeHeadCodec
        implicit val codecPars = storage.serializePars.toSizeHeadCodec
        implicit val codecCont = storage.serializeTaggedContinuation.toSizeHeadCodec
        RSpaceImporter.validateStateItems[
          F,
          Par,
          BindPattern,
          ListParWithRandom,
          TaggedContinuation
        ] _
      }
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
        populateDag(approvedBlock.candidate.block, minBlockNumberForDeployLifespan, st.heightMap)
      }

      // Run both streams in parallel until tuple space and all needed blocks are received
      _ <- fs2.Stream(blockRequestAddDagStream, tupleSpaceLogStream).parJoinUnbounded.compile.drain

      // Transition to Running state
      _ <- createCasperAndTransitionToRunning(approvedBlock)
    } yield ()
  }

  private def validateBlock(block: BlockMessage): F[Boolean] = {
    val blockNumber = ProtoUtil.blockNumber(block)
    if (blockNumber == 0L) {
      // TODO: validate genesis (zero) block correctly
      true.pure
    } else
      Validate.blockHash(block).map(_ == Right(Valid))
  }

  private def populateDag(
      startBlock: BlockMessage,
      initialMinHeight: Long,
      heightMap: SortedMap[Long, Set[BlockHash]]
  ): F[Unit] = {
    import cats.instances.list._

    def getMinBlockHeight: F[Long] = {
      val blockHashes = startBlock.justifications.map(_.latestBlockHash)
      for {
        blocks    <- blockHashes.traverse(BlockStore[F].getUnsafe)
        minHeight = if (blocks.isEmpty) 0L else blocks.map(ProtoUtil.blockNumber).min
        // We need parent of oldest latest block, that's why -1.
        // The same condition is also applied when downloading.
        // https://github.com/rchain/rchain/blob/0a09467628/casper/src/main/scala/coop/rchain/casper/engine/LastFinalizedStateBlockRequester.scala#L154
      } yield Math.min(minHeight - 1, initialMinHeight)
    }

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

      // Add sorted DAG in order from oldest to approved block
      minHeight <- getMinBlockHeight
      _ <- heightMap.flatMap(_._2).toList.traverse_ { hash =>
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

  private def createCasperAndTransitionToRunning(approvedBlock: ApprovedBlock): F[Unit] = {
    val ab = approvedBlock.candidate.block
    for {
      casper <- MultiParentCasper
                 .hashSetCasper[F](
                   validatorId,
                   ab,
                   shardId,
                   finalizationRate
                 )
      _ <- Log[F].info("MultiParentCasper instance created.")
      _ <- transitionToRunning[F](
            casper,
            approvedBlock,
            validatorId,
            ().pure,
            enableStateExporter
          )
      _ <- CommUtil[F].sendForkChoiceTipRequest
    } yield ()
  }
}
