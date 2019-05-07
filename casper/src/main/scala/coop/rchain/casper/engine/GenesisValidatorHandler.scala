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
class GenesisValidatorHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: TransportLayer: Log: Time: SafetyOracle: ErrorHandler: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage](
    runtimeManager: RuntimeManager[F],
    validatorId: ValidatorIdentity,
    shardId: String,
    blockApprover: BlockApproverProtocol
) extends CasperEngine[F] {
  import CasperEngine._
  def applicative: Applicative[F] = Applicative[F]

  override def handleApprovedBlock(
      ab: ApprovedBlock
  ): F[Option[MultiParentCasper[F]]] =
    for {
      _ <- Log[F].info("Received ApprovedBlock message while in GenesisValidatorHandler state.")
      casperO <- onApprovedBlockTransition(
                  ab,
                  Set(ByteString.copyFrom(validatorId.publicKey.bytes)),
                  runtimeManager,
                  Some(validatorId),
                  shardId
                )
      _ <- casperO.fold(Log[F].warn("MultiParentCasper instance not created."))(
            _ => Log[F].info("MultiParentCasper instance created.")
          )
    } yield casperO

  override def handleApprovedBlockRequest(
      peer: PeerNode,
      br: ApprovedBlockRequest
  ): F[Unit] =
    sendNoApprovedBlockAvailable(peer, br.identifier)

  override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit] =
    blockApprover.unapprovedBlockPacketHandler(peer, ub, runtimeManager)

  override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
    Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
}
