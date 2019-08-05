package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
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
import coop.rchain.metrics.Span.TraceId

class GenesisValidator[F[_]: Sync: Metrics: Span: Concurrent: ConnectionsCell: TransportLayer: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage: EngineCell: RuntimeManager: Running.RequestedBlocks](
    validatorId: ValidatorIdentity,
    shardId: String,
    blockApprover: BlockApproverProtocol
) extends Engine[F] {
  import Engine._
  def applicative: Applicative[F] = Applicative[F]

  override def handle(peer: PeerNode, msg: CasperMessage, traceId: TraceId): F[Unit] = msg match {
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case ub: UnapprovedBlock =>
      implicit val tid: TraceId = traceId
      blockApprover.unapprovedBlockPacketHandler(peer, ub) >> {
        val validators = Set(ByteString.copyFrom(validatorId.publicKey.bytes))
        Engine.tranistionToInitializing(shardId, Some(validatorId), validators, init = noop)
      }
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
