package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import EngineCell._
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared._
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.models.BlockHash.BlockHash

class GenesisValidator[F[_]: Sync: Metrics: Span: Concurrent: CommUtil: TransportLayer: ConnectionsCell: RPConfAsk: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: BlockStore: LastApprovedBlock: BlockDagStorage: LastFinalizedStorage: EngineCell: RuntimeManager: BlockRetriever: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage: CasperBufferStorage](
    validatorId: ValidatorIdentity,
    shardId: String,
    finalizationRate: Int,
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
              .transitionToInitializing(shardId, finalizationRate, Some(validatorId), init = noop)
          }
        )
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
