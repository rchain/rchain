package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative

import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.Metrics
import coop.rchain.shared._

import com.google.protobuf.ByteString

class GenesisValidator[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: TransportLayer: Log: EventLog: Time: SafetyOracle: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage: EngineCell: MultiParentCasperRef](
    rm: RuntimeManager[F],
    validatorId: ValidatorIdentity,
    shardId: String,
    blockApprover: BlockApproverProtocol
) extends Engine[F] {
  import Engine._
  def applicative: Applicative[F] = Applicative[F]

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case ub: UnapprovedBlock =>
      blockApprover.unapprovedBlockPacketHandler(peer, ub, rm) >> {
        val validators = Set(ByteString.copyFrom(validatorId.publicKey.bytes))
        Engine.tranistionToInitializing(rm, shardId, Some(validatorId), validators, init = noop)
      }
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
