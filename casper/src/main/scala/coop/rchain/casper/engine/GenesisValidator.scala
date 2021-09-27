package coop.rchain.casper.engine

import cats.Applicative
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
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

// format: off
class GenesisValidator[F[_]
  /* Execution */   : Concurrent: Time
  /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
  /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
  /* Rholang */     : RuntimeManager
  /* Storage */     : BlockStore: BlockDagStorage: DeployStorage: RSpaceStateManager
  /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
(
    blockDagStateRef: Ref[F, BlockDagState],
    casperShardConf: CasperShardConf,
    validatorId: ValidatorIdentity,
    blockApprover: BlockApproverProtocol,
    processBlockInRunning: BlockMessage => F[Unit]
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
                blockDagStateRef,
                casperShardConf,
                Some(validatorId),
                init = noop,
                true,
                false,
                processBlockInRunning
              )
          }
        )
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
