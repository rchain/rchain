package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.Applicative
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock

import EngineCell._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared._
import coop.rchain.shared
import com.google.protobuf.ByteString
import coop.rchain.metrics.Span.TraceId

/** Node in this state will query peers in the network with [[ApprovedBlockRequest]] message
  * and will wait for the [[ApprovedBlock]] message to arrive. Until then  it will respond with
  * `F[None]` to all other message types.
    **/
class Initializing[F[_]: Sync: Metrics: Span: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: EventLog: Time: SafetyOracle: LastFinalizedBlockCalculator: RPConfAsk: LastApprovedBlock: BlockDagStorage: EngineCell: RuntimeManager: Running.RequestedBlocks](
    shardId: String,
    validatorId: Option[ValidatorIdentity],
    validators: Set[ByteString],
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._
  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage, traceId: TraceId): F[Unit] = msg match {
    case ab: ApprovedBlock =>
      onApprovedBlockTransition(
        ab,
        validators,
        validatorId,
        shardId,
        traceId
      )
    case br: ApprovedBlockRequest     => sendNoApprovedBlockAvailable(peer, br.identifier)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }

  private def onApprovedBlockTransition(
      approvedBlock: ApprovedBlock,
      validators: Set[ByteString],
      validatorId: Option[ValidatorIdentity],
      shardId: String,
      traceId: TraceId
  ): F[Unit] =
    for {
      _       <- Log[F].info("Received ApprovedBlock message.")
      isValid <- Validate.approvedBlock[F](approvedBlock, validators)
      maybeCasper <- if (isValid) {
                      for {
                        _ <- Log[F].info("Valid ApprovedBlock received!")
                        _ <- EventLog[F].publish(
                              shared.Event.ApprovedBlockReceived(
                                approvedBlock.candidate
                                  .flatMap(
                                    _.block.map(b => PrettyPrinter.buildStringNoLimit(b.blockHash))
                                  )
                                  .getOrElse("")
                              )
                            )
                        genesis = approvedBlock.candidate.flatMap(_.block).get
                        _       <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
                        _       <- LastApprovedBlock[F].set(approvedBlock)
                        casper <- MultiParentCasper
                                   .hashSetCasper[F](validatorId, genesis, shardId, traceId)
                        _ <- Engine
                              .transitionToRunning[F](casper, approvedBlock, ().pure[F])
                        _ <- CommUtil.sendForkChoiceTipRequest[F]
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
