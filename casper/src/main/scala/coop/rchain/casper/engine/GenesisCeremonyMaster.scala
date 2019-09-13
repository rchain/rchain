package coop.rchain.casper.engine

import scala.concurrent.duration.FiniteDuration
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import EngineCell._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
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

class GenesisCeremonyMaster[F[_]: Sync: ConnectionsCell: BlockStore: TransportLayer: Log: Time: SafetyOracle: RPConfAsk: LastApprovedBlock](
    approveProtocol: ApproveBlockProtocol[F]
) extends Engine[F] {
  import Engine._
  def applicative: Applicative[F] = Applicative[F]

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
  def approveBlockInterval[F[_]: Sync: Metrics: Span: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: RPConfAsk: LastApprovedBlock: BlockDagStorage: EngineCell: RuntimeManager: Running.RequestedBlocks: EventPublisher: SynchronyConstraintChecker](
      interval: FiniteDuration,
      shardId: String,
      validatorId: Option[ValidatorIdentity]
  ): F[Unit] =
    for {
      _                  <- Time[F].sleep(interval)
      lastApprovedBlockO <- LastApprovedBlock[F].get
      cont <- lastApprovedBlockO match {
               case None =>
                 approveBlockInterval[F](interval, shardId, validatorId)
               case Some(approvedBlock) =>
                 val genesis = approvedBlock.candidate.block
                 for {
                   _ <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
                   casper <- MultiParentCasper
                              .hashSetCasper[F](validatorId, genesis, shardId)
                   _ <- Engine
                         .transitionToRunning[F](casper, approvedBlock, ().pure[F])
                   _ <- CommUtil.sendForkChoiceTipRequest[F]
                 } yield ()
             }
    } yield cont
}
