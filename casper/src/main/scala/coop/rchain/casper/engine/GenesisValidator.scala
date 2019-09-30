package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage

class GenesisValidator[F[_]: Sync: Metrics: Span: Concurrent: ConnectionsCell: TransportLayer: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage: EngineCell: RuntimeManager: Running.RequestedBlocks: EventPublisher: SynchronyConstraintChecker](
    validatorId: ValidatorIdentity,
    shardId: String,
    blockApprover: BlockApproverProtocol
) extends Engine[F] {
  import Engine._
  private val F    = Applicative[F]
  private val noop = F.unit

  override val init = noop
  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case ub: UnapprovedBlock =>
      blockApprover.unapprovedBlockPacketHandler(peer, ub) >> {
        Engine.transitionToInitializing(shardId, Some(validatorId), init = noop)
      }
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
