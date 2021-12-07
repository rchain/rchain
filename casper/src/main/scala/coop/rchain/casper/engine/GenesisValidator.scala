package coop.rchain.casper.engine

import cats.Applicative
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared._
import fs2.concurrent.Queue

// format: off
class GenesisValidator[F[_]
  /* Execution */   : Concurrent: Time: Timer
  /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
  /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
  /* Rholang */     : RuntimeManager
  /* Casper */      : Estimator: SafetyOracle: LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
  /* Storage */     : BlockStore: BlockDagStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
  /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
(
    blockProcessingQueue: Queue[F, (Casper[F], BlockMessage)],
    blocksInProcessing: Ref[F, Set[BlockHash]],
    casperShardConf: CasperShardConf,
    validatorId: ValidatorIdentity,
    blockApprover: BlockApproverProtocol
) extends Engine[F] {
  import Engine._
  private val F    = Applicative[F]
  private val noop = F.unit

  private val seenCandidates                          = Cell.unsafe[F, Map[BlockHash, Boolean]](Map.empty)
  private def isRepeated(hash: BlockHash): F[Boolean] = seenCandidates.reads(_.contains(hash))
  private def ack(hash: BlockHash): F[Unit]           = seenCandidates.modify(_ + (hash -> true))

  override val init = noop
  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case ub: UnapprovedBlock =>
      isRepeated(ub.candidate.block.blockHash)
        .ifM(
          Log[F].warn(
            s"UnapprovedBlock ${PrettyPrinter.buildString(ub.candidate.block.blockHash)} is already being verified. " +
              s"Dropping repeated message."
          ),
          ack(ub.candidate.block.blockHash) >> blockApprover
            .unapprovedBlockPacketHandler(peer, ub) >> {
            Engine
              .transitionToInitializing(
                blockProcessingQueue,
                blocksInProcessing,
                casperShardConf,
                Some(validatorId),
                init = noop
              )
          }
        )
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
