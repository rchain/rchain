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

/** Node in this state will query peers in the network with [[ApprovedBlockRequest]] message
  * and will wait for the [[ApprovedBlock]] message to arrive. Until then  it will respond with
  * `F[None]` to all other message types.
    **/
class BootstrapCasperHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: BlockDagStorage: MultiParentCasperRef: EngineCell](
    runtimeManager: RuntimeManager[F],
    shardId: String,
    validatorId: Option[ValidatorIdentity],
    validators: Set[ByteString],
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._
  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlockTransition(
        ab,
        validators,
        runtimeManager,
        validatorId,
        shardId
      )
    case br: ApprovedBlockRequest     => sendNoApprovedBlockAvailable(peer, br.identifier)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }

}
