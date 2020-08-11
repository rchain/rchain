package coop.rchain.casper.engine

import cats.effect.Concurrent
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
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared
import coop.rchain.shared._
import fs2.concurrent.Queue

import scala.concurrent.duration._

object Initializing {
  val pageSize = 3000

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
    trimState: Boolean = false
) extends Engine[F] {

  import Engine._

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock        => onApprovedBlock(peer, ab.candidate.block)
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case na: NoApprovedBlockAvailable =>
      logNoApprovedBlockAvailable[F](na.nodeIdentifer) >>
        Time[F].sleep(10.seconds) >>
        CommUtil[F].requestApprovedBlock(trimState)
    case s: StoreItemsMessage =>
      tupleSpaceQueue.enqueue1(s)

    case b: BlockMessage =>
      Log[F].info(s"BlockMessage received ${PrettyPrinter.buildString(b)}") *>
        blockMessageQueue.enqueue1(b)

    case _ => ().pure
  }

  /*private def onApprovedBlock(
      sender: PeerNode,
      approvedBlock: ApprovedBlock
  ): F[Unit] = {
    val senderIsBootstrap = RPConfAsk[F].ask.map(_.bootstrap.exists(_ == sender))
    for {
      _       <- Log[F].info("Received ApprovedBlock message.")
      isValid <- senderIsBootstrap &&^ Validate.approvedBlock[F](approvedBlock)
      _ <- if (isValid) {
            for {
              _       <- Log[F].info("Valid ApprovedBlock received!")
              genesis = approvedBlock.candidate.block
              _ <- EventLog[F].publish(
                    shared.Event.ApprovedBlockReceived(
                      PrettyPrinter
                        .buildStringNoLimit(genesis.blockHash)
                    )
                  )
              _ <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
              _ <- LastApprovedBlock[F].set(approvedBlock)
              // Request last finalized block from bootstrap node
              _ <- CommUtil[F].requestLastFinalizedBlock
            } yield ()
          } else
            Log[F].info("Invalid ApprovedBlock received; refusing to add.")

    } yield ()
  }*/

  private def onApprovedBlock(sender: PeerNode, approvedBlock: BlockMessage): F[Unit] = {
    val senderIsBootstrap = RPConfAsk[F].ask.map(_.bootstrap.exists(_ == sender))
    for {
      // TODO: resolve validation of received approved block
      isValid <- senderIsBootstrap // &&^ Validate.approvedBlock[F](approvedBlock)
      _ <- if (isValid) {
            for {
              _ <- Log[F].info(
                    s"Valid Last Finalized Block received ${PrettyPrinter.buildString(approvedBlock)}"
                  )
              _ <- Log[F].warn(
                    s"JUSTIF: ${approvedBlock.justifications.map(_.latestBlockHash).map(PrettyPrinter.buildString).mkString(", ")}"
                  )

              _ <- EventLog[F].publish(
                    shared.Event.ApprovedBlockReceived(
                      PrettyPrinter
                        .buildStringNoLimit(approvedBlock.blockHash)
                    )
                  )

              // Add last finalized block as ApprovedBlock
              ab = ApprovedBlock(ApprovedBlockCandidate(approvedBlock, 0), Nil)
              _  <- insertIntoBlockAndDagStore[F](approvedBlock, ab)
              _  <- LastApprovedBlock[F].set(ab)

              // Update last finalized block with received block hash
              _ <- LastFinalizedStorage[F].put(approvedBlock.blockHash)

              // Transition to restore last finalized state
              _ <- requestApprovedState
            } yield ()
          } else
            Log[F].info("Invalid LastFinalizedBlock received; refusing to add.")
    } yield ()
  }

  def requestApprovedState: F[Unit] = {

    import cats.instances.list._
    import coop.rchain.catscontrib.Catscontrib.ToBooleanF
    def addBlockToDag(block: BlockMessage, approvedBlock: ApprovedBlock): F[Unit] =
      for {
        dag   <- BlockDagStorage[F].getRepresentation
        added <- dag.contains(block.blockHash)
        _ <- BlockDagStorage[F]
              .insert(
                block,
                approvedBlock.candidate.block,
                invalid = false
              )
              .unlessA(added)
        missingDeps <- ProtoUtil.dependenciesHashesOf(block).filterA(dag.contains(_).not)
        blocksToAdd <- missingDeps.traverse(BlockStore[F].get)
        sortedDeps = blocksToAdd.flatten.sortWith(
          _.body.state.blockNumber > _.body.state.blockNumber
        )
        _ <- sortedDeps.traverse(addBlockToDag(_, approvedBlock))
      } yield ()

    for {
      // Approved block is the starting point to request the state
      approvedBlock <- LastApprovedBlock[F].get >>= (_.liftTo(
                        ApprovedBlockNotAvailableWhenRestoringLastFinalizedStateError
                      ))

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

      // Populate DAG with blocks retrieved
      _ <- addBlockToDag(approvedBlock.candidate.block, approvedBlock)

      _ <- Log[F].info(
            s"Approved State received ${PrettyPrinter.buildString(approvedBlock.candidate.block)}, transitioning to Running state."
          )

      // Transition to Running state
      _ <- createCasperAndTransitionToRunning(approvedBlock)
    } yield ()
  }

  private def createCasperAndTransitionToRunning(approvedBlock: ApprovedBlock): F[Unit] = {
    val genesis = approvedBlock.candidate.block
    for {
      casper <- MultiParentCasper
                 .hashSetCasper[F](
                   validatorId,
                   genesis,
                   shardId,
                   finalizationRate,
                   skipValidateGenesis = true
                 )
      _ <- Log[F].info("MultiParentCasper instance created.")
      _ <- transitionToRunning[F](
            casper,
            approvedBlock,
            validatorId,
            ().pure
          )
      _ <- CommUtil[F].sendForkChoiceTipRequest
    } yield ()
  }
}
