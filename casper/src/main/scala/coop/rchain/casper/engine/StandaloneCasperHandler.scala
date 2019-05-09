package coop.rchain.casper.engine

import EngineCell._
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

/** Node in this state is will send out an [[UnapprovedBlock]] message to all peers
  * and will wait for [[BlockApproval]] messages forwarding handling of those to instance of [[ApproveBlockProtocol]] class.
  * After enough [[BlockApproval]]s has been received it will create an [[ApprovedBlock]] and send it to peers.
  *
  *
  * For all other messages it will return `F[None]`.
    **/
class StandaloneCasperHandler[F[_]: Sync: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock](
    approveProtocol: ApproveBlockProtocol[F]
) extends CasperEngine[F] {
  import CasperEngine._
  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = approveProtocol.run()

  override def handleApprovedBlockRequest(
      peer: PeerNode,
      br: ApprovedBlockRequest
  ): F[Unit] =
    sendNoApprovedBlockAvailable(peer, br.identifier)

  override def handleBlockApproval(ba: BlockApproval): F[Unit] =
    approveProtocol.addApproval(ba)

  override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
    Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
}

object StandaloneCasperHandler {
  import CasperEngine._
  def approveBlockInterval[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: MultiParentCasperRef: BlockDagStorage: EngineCell](
      interval: FiniteDuration,
      shardId: String,
      runtimeManager: RuntimeManager[F],
      validatorId: Option[ValidatorIdentity]
  ): F[Unit] =
    for {
      _                  <- Time[F].sleep(interval)
      lastApprovedBlockO <- LastApprovedBlock[F].get
      cont <- lastApprovedBlockO match {
               case None =>
                 approveBlockInterval[F](
                   interval,
                   shardId,
                   runtimeManager,
                   validatorId
                 )
               case Some(approvedBlock) =>
                 val genesis = approvedBlock.candidate.flatMap(_.block).get
                 for {
                   _ <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
                   casper <- MultiParentCasper.hashSetCasper[F](
                              runtimeManager,
                              validatorId,
                              genesis,
                              shardId
                            )
                   _ <- CasperEngine
                         .transitionToApprovedBlockReceivedHandler[F](casper, approvedBlock)
                   _ <- CommUtil.sendForkChoiceTipRequest[F]
                 } yield ()
             }
    } yield cont
}
