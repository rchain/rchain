package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import EngineCell._
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared._
import coop.rchain.shared
import scala.concurrent.duration._
import scala.language.higherKinds

/** Node in this state will query peers in the network with [[ApprovedBlockRequest]] message
  * and will wait for the [[ApprovedBlock]] message to arrive. Until then  it will respond with
  * `F[None]` to all other message types.
    **/
class Initializing[F[_]: Sync: Metrics: Span: Concurrent: BlockStore: CommUtil: TransportLayer: ConnectionsCell: RPConfAsk: BlockRetriever: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: LastApprovedBlock: BlockDagStorage: LastFinalizedStorage: EngineCell: RuntimeManager: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage: CasperBufferStorage](
    shardId: String,
    finalizationRate: Int,
    validatorId: Option[ValidatorIdentity],
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._
  private val F    = Applicative[F]
  private val noop = F.unit

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlockTransition(
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
    case _ => noop
  }

  private def onApprovedBlockTransition(
      approvedBlock: ApprovedBlock,
      validatorId: Option[ValidatorIdentity],
      shardId: String,
      finalizationRate: Int
  ): F[Unit] =
    for {
      _       <- Log[F].info("Received ApprovedBlock message.")
      isValid <- Validate.approvedBlock[F](approvedBlock)
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
