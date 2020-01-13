package coop.rchain.casper.engine

import scala.concurrent.duration.FiniteDuration
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import EngineCell._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared._

class GenesisCeremonyMaster[F[_]: Sync: BlockStore: CommUtil: TransportLayer: RPConfAsk: Log: Time: SafetyOracle: LastApprovedBlock](
    approveProtocol: ApproveBlockProtocol[F]
) extends Engine[F] {
  import Engine._
  private val F    = Applicative[F]
  private val noop = F.unit

  override def init: F[Unit] = approveProtocol.run()

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case br: ApprovedBlockRequest     => sendNoApprovedBlockAvailable(peer, br.identifier)
    case ba: BlockApproval            => approveProtocol.addApproval(ba)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }
}

object GenesisCeremonyMaster {
  import Engine._
  def approveBlockInterval[F[_]: Sync: Metrics: Span: Concurrent: CommUtil: TransportLayer: ConnectionsCell: RPConfAsk: Running.RequestedBlocks: BlockStore: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: LastApprovedBlock: BlockDagStorage: LastFinalizedStorage: EngineCell: RuntimeManager: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage](
      interval: FiniteDuration,
      shardId: String,
      finalizationRate: Int,
      validatorId: Option[ValidatorIdentity]
  ): F[Unit] =
    for {
      _                  <- Time[F].sleep(interval)
      lastApprovedBlockO <- LastApprovedBlock[F].get
      cont <- lastApprovedBlockO match {
               case None =>
                 approveBlockInterval[F](interval, shardId, finalizationRate, validatorId)
               case Some(approvedBlock) =>
                 val genesis = approvedBlock.candidate.block
                 for {
                   _ <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
                   casper <- MultiParentCasper
                              .hashSetCasper[F](validatorId, genesis, shardId, finalizationRate)
                   _ <- Engine
                         .transitionToRunning[F](casper, approvedBlock, validatorId, ().pure[F])
                   _ <- CommUtil[F].sendForkChoiceTipRequest
                 } yield ()
             }
    } yield cont
}
