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
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.engine.Initializing._
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
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared
import coop.rchain.shared._
import fs2.concurrent.Queue

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

object Initializing {
  // Approved block must be available before restoring last finalized state
  final case object ApprovedBlockNotAvailableWhenRestoringLastFinalizedStateError extends Exception
}

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
      Log[F].info(s"Received ${s.pretty}") *> tupleSpaceQueue.enqueue1(s)

    case b: BlockMessage =>
      Log[F].info(s"BlockMessage received ${PrettyPrinter.buildString(b)}") *>
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
              s"Valid approved block ${PrettyPrinter
                .buildString(approvedBlock.candidate.block, short = true)} received. Restoring approved state."
            )

        // Download approved state and all related blocks
        _ <- requestApprovedState(approvedBlock)

        _ <- EventLog[F].publish(
              shared.Event.ApprovedBlockReceived(
                PrettyPrinter
                  .buildStringNoLimit(block.blockHash)
              )
            )

        _ <- BlockStore[F].putApprovedBlock(approvedBlock)
        _ <- LastApprovedBlock[F].set(approvedBlock)

        // Update last finalized block with received block hash
        _ <- LastFinalizedStorage[F].put(block.blockHash)

        _ <- Log[F].info(
              s"Approved state for block ${PrettyPrinter
                .buildString(approvedBlock.candidate.block, short = true)} is successfully restored."
            )
      } yield ()
    }

    for {
      // TODO resolve validation of approved block - we should be sure that bootstrap is not lying
      // Might be Validate.approvedBlock is enough but have to check
      isValid <- senderIsBootstrap &&^ Validate.approvedBlock[F](approvedBlock)

      _ <- Log[F].info("Invalid LastFinalizedBlock received; refusing to add.").whenA(!isValid)

      // Start only once, when state is true and approved block is valid
      start <- startRequester.modify {
                case true if isValid  => (false, true)
                case true if !isValid => (true, false)
                case _                => (false, false)
              }

      // HACK: Wait for master transition to Running
      _ <- Time[F].sleep(3.seconds).whenA(start)

      _ <- handleApprovedBlock.whenA(start)
    } yield ()
  }

  def requestApprovedState(approvedBlock: ApprovedBlock): F[Unit] =
    for {
      // Request all blocks for Last Finalized State
      blockRequestStream <- LastFinalizedStateBlockRequester.stream(
                             approvedBlock,
                             blockMessageQueue
                           )

      // Request tuple space state for Last Finalized State
      tupleSpaceStream <- LastFinalizedStateTupleSpaceRequester.stream(
                           approvedBlock,
                           tupleSpaceQueue
                         )

      // Execute stream until tuple space and all needed blocks are received
      _ <- fs2.Stream(blockRequestStream, tupleSpaceStream).parJoinUnbounded.compile.drain

      // Approved block is saved after the whole state is received
      //  to restart requesting if interrupted with incomplete state.
      _ <- BlockStore[F].put(approvedBlock.candidate.block)

      // Populate DAG with blocks retrieved
      _ <- populateDag(approvedBlock.candidate.block)

      // Transition to Running state
      _ <- createCasperAndTransitionToRunning(approvedBlock)
    } yield ()

  private def populateDag(startBlock: BlockMessage): F[Unit] = {
    import cats.instances.list._

    type SortedBlocks = SortedMap[Long, Set[BlockHash]]
    type RecParams    = (Seq[BlockHash], Set[BlockHash], SortedBlocks)

    def loopDependencies(params: RecParams): F[Either[RecParams, SortedBlocks]] = {
      val (hashes, skipped, sorted) = params
      hashes match {
        case Nil => sorted.asRight[RecParams].pure[F]
        case head +: tail =>
          for {
            block       <- BlockStore[F].getUnsafe(head)
            depsAll     = ProtoUtil.dependenciesHashesOf(block)
            depsNext    <- depsAll.filter(skipped).filterA(BlockStore[F].contains)
            depsSkipped = depsAll.filter(depsNext.contains)
            blockNumber = block.body.state.blockNumber
            nrHashes    = sorted.getOrElse(blockNumber, Set())
            // Data for continue recursion
            newRest    = tail ++ depsNext
            newSkipped = skipped ++ depsSkipped
            newSorted  = sorted.updated(blockNumber, nrHashes + block.blockHash)
          } yield (newRest, newSkipped, newSorted).asLeft
      }
    }

    val emptySorted = SortedMap[Long, Set[BlockHash]]()
    for {
      sortedHashes <- (Seq(startBlock.blockHash), Set[BlockHash](), emptySorted)
                       .tailRecM(loopDependencies)
      // Add sorted DAG in reverse order (from approved block)
      _ <- sortedHashes.flatMap(_._2).toList.reverse.traverse_ { hash =>
            for {
              block <- BlockStore[F].getUnsafe(hash)
              _     <- BlockDagStorage[F].insert(block, invalid = false)
            } yield ()
          }
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
