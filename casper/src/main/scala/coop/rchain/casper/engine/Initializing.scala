package coop.rchain.casper.engine

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared
import coop.rchain.shared._

import scala.concurrent.duration._

/** Node in this state will query peers in the network with [[ApprovedBlockRequest]] message
  * and will wait for the [[ApprovedBlock]] message to arrive. Until then  it will respond with
  * `F[None]` to all other message types.
  * */
// format: off
class Initializing[F[_]
  /* Execution */   : Concurrent: Time
  /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
  /* State */       : EngineCell: RPConfAsk: ConnectionsCell: LastApprovedBlock
  /* Rholang */     : RuntimeManager
  /* Casper */      : Estimator: SafetyOracle: LastFinalizedBlockCalculator: LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
  /* Storage */     : BlockStore: BlockDagStorage: LastFinalizedStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
  /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
(shardId: String, finalizationRate: Int, validatorId: Option[ValidatorIdentity], theInit: F[Unit])
    extends Engine[F] {

  import Engine._

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlockTransition(
        peer,
        ab,
        validatorId,
        shardId,
        finalizationRate
      )
    case br: ApprovedBlockRequest => sendNoApprovedBlockAvailable(peer, br.identifier)
    case na: NoApprovedBlockAvailable =>
      logNoApprovedBlockAvailable[F](na.nodeIdentifer) >>
        Time[F].sleep(10.seconds) >>
        CommUtil[F].requestApprovedBlock
    case LastFinalizedBlock(block) => onLastFinalizedBlock(peer, block)
    case _                         => ().pure
  }

  private def onApprovedBlockTransition(
      sender: PeerNode,
      approvedBlock: ApprovedBlock,
      validatorId: Option[ValidatorIdentity],
      shardId: String,
      finalizationRate: Int
  ): F[Unit] = {
    val senderIsBootstrap = RPConfAsk[F].ask.map(_.bootstrap.exists(_ == sender))
    for {
      _       <- Log[F].info("Received ApprovedBlock message.")
      isValid <- senderIsBootstrap &&^ Validate.approvedBlock[F](approvedBlock)
      maybeCasper <- if (isValid) {
                      for {
                        _ <- Log[F].info("Valid ApprovedBlock received!")
                        _ <- EventLog[F].publish(
                              shared.Event.ApprovedBlockReceived(
                                PrettyPrinter
                                  .buildStringNoLimit(approvedBlock.candidate.block.blockHash)
                              )
                            )
                        genesis = approvedBlock.candidate.block
                        _       <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
                        _       <- LastApprovedBlock[F].set(approvedBlock)
                        casper <- MultiParentCasper
                                   .hashSetCasper[F](
                                     validatorId,
                                     genesis,
                                     shardId,
                                     finalizationRate,
                                     skipValidateGenesis = false
                                   )
                        _ <- Engine
                              .transitionToRunning[F](
                                casper,
                                approvedBlock,
                                validatorId,
                                ().pure[F]
                              )
                        _ <- CommUtil[F].sendForkChoiceTipRequest
                      } yield Option(casper)
                    } else
                      Log[F]
                        .info("Invalid ApprovedBlock received; refusing to add.")
                        .map(_ => none[MultiParentCasper[F]])
      _ <- maybeCasper.fold(Log[F].warn("MultiParentCasper instance not created."))(
            _ => Log[F].info("MultiParentCasper instance created.")
          )

    } yield ()
  }

  private def onLastFinalizedBlock(sender: PeerNode, lastFinalizedBlock: BlockMessage): F[Unit] = {
    val senderIsBootstrap = RPConfAsk[F].ask.map(_.bootstrap.exists(_ == sender))
    for {
      isValid <- senderIsBootstrap
      _ <- if (isValid) {
            for {
              _ <- Log[F].info(
                    s"Valid Last Finalized Block received ${PrettyPrinter.buildString(lastFinalizedBlock)}"
                  )
              _ <- Log[F].warn(
                    s"JUSTIF: ${lastFinalizedBlock.justifications.map(_.latestBlockHash).map(PrettyPrinter.buildString).mkString(", ")}"
                  )

              _ <- EventLog[F].publish(
                    shared.Event.ApprovedBlockReceived(
                      PrettyPrinter
                        .buildStringNoLimit(lastFinalizedBlock.blockHash)
                    )
                  )

              // TODO: resolve validation of received last finalized block
              // Add last finalized block as ApprovedBlock
              approvedBlock = ApprovedBlock(ApprovedBlockCandidate(lastFinalizedBlock, 0), Nil)
              _             <- insertIntoBlockAndDagStore[F](lastFinalizedBlock, approvedBlock)
              _             <- LastApprovedBlock[F].set(approvedBlock)

              // Update last finalized block with received block hash
              _ <- LastFinalizedStorage[F].put(lastFinalizedBlock.blockHash)

              // Transition to restore last finalized state
              _ <- transitionToLastFinalizedState(
                    shardId,
                    finalizationRate,
                    validatorId
                  )
            } yield ()
          } else
            Log[F].info("Invalid LastFinalizedBlock received; refusing to add.")
    } yield ()
  }

}
