package coop.rchain.casper.engine

import cats.Applicative
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.effect.{Concurrent, Sync, Timer}
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared._
import fs2.concurrent.Queue

import scala.concurrent.duration._

class GenesisCeremonyMaster[F[_]: Sync: BlockStore: CommUtil: TransportLayer: RPConfAsk: Log: Time: LastApprovedBlock](
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

// format: off
object GenesisCeremonyMaster {
  import Engine._
  def waitingForApprovedBlockLoop[F[_]
    /* Execution */   : Concurrent: Time: Timer
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Casper */      : LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
    /* Storage */     : BlockStore: ApprovedStore: BlockDagStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      blockProcessingQueue: Queue[F, (Casper[F], BlockMessage)],
      blocksInProcessing: Ref[F, Set[BlockHash]],
      casperShardConf: CasperShardConf,
      validatorId: Option[ValidatorIdentity],
      disableStateExporter: Boolean
  ): F[Unit] =
    for {
      // This loop sleep can be short as it does not do anything except checking if there is last approved block available
      _                  <- Time[F].sleep(2.seconds)
      lastApprovedBlockO <- LastApprovedBlock[F].get
      cont <- lastApprovedBlockO match {
               case None =>
                 waitingForApprovedBlockLoop[F](
                   blockProcessingQueue: Queue[F, (Casper[F], BlockMessage)],
                   blocksInProcessing: Ref[F, Set[BlockHash]],
                   casperShardConf,
                   validatorId,
                   disableStateExporter
                 )
               case Some(approvedBlock) =>
                 val ab = approvedBlock.candidate.block
                 for {
                   _ <- insertIntoBlockAndDagStore[F](ab, approvedBlock)
                   casper <- MultiParentCasper
                              .hashSetCasper[F](validatorId, casperShardConf: CasperShardConf)
                   _ <- Engine
                         .transitionToRunning[F](
                           blockProcessingQueue,
                           blocksInProcessing,
                           casper,
                           approvedBlock,
                           validatorId,
                           ().pure[F],
                           disableStateExporter
                         )
                   _ <- CommUtil[F].sendForkChoiceTipRequest
                 } yield ()
             }
    } yield cont
}
