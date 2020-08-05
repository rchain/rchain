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
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared.{EventLog, EventPublisher, Log, Time}
import fs2.concurrent.Queue

object LastFinalizedState {
  // Approved block must be available before restoring last finalized state
  final case object ApprovedBlockNotAvailableWhenRestoringLastFinalizedStateError extends Exception
}

// format: off
class LastFinalizedState[F[_]
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
    blockMessageQueue: Queue[F, BlockMessage],
    tupleSpaceQueue: Queue[F, StoreItemsMessage]
) extends Engine[F] {
  import LastFinalizedState._

  override def init: F[Unit] = ().pure[F]

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case s: StoreItemsMessage =>
      tupleSpaceQueue.enqueue1(s)

    case b: BlockMessage =>
      Log[F].info(s"BlockMessage received ${PrettyPrinter.buildString(b)}") *>
        blockMessageQueue.enqueue1(b)

    case _ => ().pure
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
                   skipValidateGenesis = false
                 )
      _ <- Log[F].info("MultiParentCasper instance created.")
      _ <- Engine
            .transitionToRunning[F](
              casper,
              approvedBlock,
              validatorId,
              ().pure
            )
      _ <- CommUtil[F].sendForkChoiceTipRequest
    } yield ()
  }

  def start =
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

      _ <- Log[F].info(
            s"Last Finalized State received ${PrettyPrinter.buildString(approvedBlock.candidate.block)}, transitioning to Running state."
          )

      // Transition to Running state
      _ <- createCasperAndTransitionToRunning(approvedBlock)
    } yield ()

}
