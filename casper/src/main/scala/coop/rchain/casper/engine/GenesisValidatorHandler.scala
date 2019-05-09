package coop.rchain.casper.engine

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** Node in this state is a genesis block validator. It will respond only to
  * [[UnapprovedBlock]] messages forwarding the logic of handling this message to
  * instance of [[BlockApproverProtocol]] class.
  *
  * When in this state node can't handle any other message type so it will return `F[None]`
    **/
class GenesisValidatorHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: TransportLayer: Log: Time: SafetyOracle: ErrorHandler: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage: EngineCell: MultiParentCasperRef](
    runtimeManager: RuntimeManager[F],
    validatorId: ValidatorIdentity,
    shardId: String,
    blockApprover: BlockApproverProtocol
) extends Engine[F] {
  import Engine._
  def applicative: Applicative[F] = Applicative[F]

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlockTransition(
        ab,
        Set(ByteString.copyFrom(validatorId.publicKey.bytes)),
        runtimeManager,
        Some(validatorId),
        shardId
      )
    case br: ApprovedBlockRequest     => sendNoApprovedBlockAvailable(peer, br.identifier)
    case ub: UnapprovedBlock          => blockApprover.unapprovedBlockPacketHandler(peer, ub, runtimeManager)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}
