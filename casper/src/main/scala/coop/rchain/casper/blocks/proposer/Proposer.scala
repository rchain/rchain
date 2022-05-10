package coop.rchain.casper.blocks.proposer

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.crypto.PrivateKey
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.{EventPublisher, Log, Stopwatch, Time}
import fs2.Stream

sealed abstract class ProposerResult
object ProposerEmpty                                                         extends ProposerResult
final case class ProposerSuccess(status: ProposeStatus, block: BlockMessage) extends ProposerResult
final case class ProposerFailure(status: ProposeStatus, seqNumber: Long)     extends ProposerResult
final case class ProposerStarted(seqNumber: Long)                            extends ProposerResult

object ProposerResult {
  def empty: ProposerResult = ProposerEmpty
  def success(status: ProposeStatus, block: BlockMessage): ProposerResult =
    ProposerSuccess(status, block)
  def failure(status: ProposeStatus, seqNumber: Long): ProposerResult =
    ProposerFailure(status, seqNumber)
  def started(seqNumber: Long): ProposerResult = ProposerStarted(seqNumber)
}

class Proposer[F[_]: Concurrent: Log: Span](
    // base state on top of which block will be created
    getCasperSnapshot: F[CasperSnapshot],
    // propose constraint checkers
    checkActiveValidator: (
        CasperSnapshot,
        ValidatorIdentity
    ) => CheckProposeConstraintsResult,
    checkEnoughBaseStake: CasperSnapshot => F[CheckProposeConstraintsResult],
    checkFinalizedHeight: CasperSnapshot => F[CheckProposeConstraintsResult],
    createBlock: (
        CasperSnapshot,
        ValidatorIdentity
    ) => F[BlockCreatorResult],
    validateBlock: (CasperSnapshot, BlockMessage) => F[ValidBlockProcessing],
    proposeEffect: BlockMessage => F[Unit],
    validator: ValidatorIdentity
) {

  implicit val RuntimeMetricsSource: Source = Metrics.Source(CasperMetricsSource, "proposer")
  // This is the whole logic of propose
  private def doPropose(s: CasperSnapshot): F[(ProposeResult, Option[BlockMessage])] =
    Span[F].traceI("do-propose") {
      for {
        // check if node is allowed to propose a block
        chk <- checkProposeConstraints(s)
        r <- chk match {
              case v: CheckProposeConstraintsFailure =>
                (ProposeResult.failure(v), none[BlockMessage]).pure[F]
              case CheckProposeConstraintsSuccess =>
                for {
                  b <- createBlock(s, validator)
                  r <- b match {
                        case NoNewDeploys =>
                          (ProposeResult.failure(NoNewDeploys), none[BlockMessage]).pure[F]
                        case Created(b) =>
                          validateBlock(s, b).flatMap {
                            case Right(v) =>
                              proposeEffect(b) >>
                                (ProposeResult.success(v), b.some).pure[F]
                            case Left(v) =>
                              Concurrent[F].raiseError[(ProposeResult, Option[BlockMessage])](
                                new Throwable(
                                  s"Validation of self created block failed with reason: $v, cancelling propose."
                                )
                              )
                          }
                      }
                } yield r
            }
      } yield r
    }

  // Check if proposer can issue a block
  private def checkProposeConstraints(
      s: CasperSnapshot
  ): F[CheckProposeConstraintsResult] =
    checkActiveValidator(s, validator) match {
      case NotBonded => CheckProposeConstraintsResult.notBonded.pure[F]
      case _ =>
        val work = Stream(
          Stream.eval[F, CheckProposeConstraintsResult](checkEnoughBaseStake(s)),
          Stream.eval[F, CheckProposeConstraintsResult](checkFinalizedHeight(s))
        )
        work
          .parJoin(2)
          .compile
          .toList
          // pick some result that is not Success, or return Success
          .map(
            _.find(_ != CheckProposeConstraintsSuccess).getOrElse(CheckProposeConstraintsSuccess)
          )
    }

  def propose(
      isAsync: Boolean,
      proposeIdDef: Deferred[F, ProposerResult]
  ): F[(ProposeResult, Option[BlockMessage])] = {
    def getValidatorNextSeqNumber(cs: CasperSnapshot): Long = {
      val valBytes = ByteString.copyFrom(validator.publicKey.bytes)
      cs.maxSeqNums.getOrElse(valBytes, 0L) + 1L
    }
    for {
      // get snapshot to serve as a base for propose
      s <- Stopwatch.time(Log[F].info(_))(s"getCasperSnapshot")(getCasperSnapshot)
      result <- if (isAsync) for {
                 nextSeq <- getValidatorNextSeqNumber(s).pure[F]
                 _       <- proposeIdDef.complete(ProposerResult.started(nextSeq))

                 // propose
                 r <- doPropose(s)
               } yield r
               else
                 for {
                   // propose
                   r <- doPropose(s)

                   (result, blockHashOpt) = r
                   proposerResult = blockHashOpt.fold {
                     val seqNumber = getValidatorNextSeqNumber(s)
                     ProposerResult.failure(result.proposeStatus, seqNumber)
                   } { block =>
                     ProposerResult.success(result.proposeStatus, block)
                   }
                   _ <- proposeIdDef.complete(proposerResult)
                 } yield r

    } yield result
  }
}

object Proposer {
  // format: off
  def apply[F[_]
    /* Execution */   : Concurrent: Timer: Time
    /* Casper */      : SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker
    /* Storage */     : BlockStore: BlockDagStorage
    /* Diagnostics */ : Log: Span: Metrics: EventPublisher
    /* Comm */        : CommUtil: BlockRetriever: RuntimeManager
  ] // format: on
  (
      validatorIdentity: ValidatorIdentity,
      casperShardConf: CasperShardConf,
      dummyDeployOpt: Option[(PrivateKey, String)] = None
  ): Proposer[F] = {
    val getCasperSnapshotSnapshot = MultiParentCasper.getSnapshot[F](casperShardConf)

    val createBlock = (s: CasperSnapshot, validatorIdentity: ValidatorIdentity) =>
      BlockCreator.create(s, validatorIdentity, dummyDeployOpt)

    val validateBlock = (s: CasperSnapshot, b: BlockMessage) => MultiParentCasper.validate(b, s)

    val checkValidatorIsActive = (s: CasperSnapshot, validator: ValidatorIdentity) =>
      if (s.onChainState.activeValidators.contains(ByteString.copyFrom(validator.publicKey.bytes)))
        CheckProposeConstraintsSuccess
      else
        NotBonded

    val checkEnoughBaseStake = (s: CasperSnapshot) =>
      SynchronyConstraintChecker[F].check(
        s,
        RuntimeManager[F],
        validatorIdentity
      )

    val checkLastFinalizedHeightConstraint = (s: CasperSnapshot) =>
      LastFinalizedHeightConstraintChecker[F].check(
        s,
        validatorIdentity
      )

    val proposeEffect = (b: BlockMessage) =>
      // store block
      BlockStore[F].put(b) >>
        // save changes to Casper
        MultiParentCasper.handleValidBlock(b) >>
        // inform block retriever about block
        BlockRetriever[F].ackInCasper(b.blockHash) >>
        // broadcast hash to peers
        CommUtil[F].sendBlockHash(b.blockHash, b.sender) >>
        // Publish event
        EventPublisher[F].publish(MultiParentCasper.createdEvent(b))

    new Proposer(
      getCasperSnapshotSnapshot,
      checkValidatorIsActive,
      checkEnoughBaseStake,
      checkLastFinalizedHeightConstraint,
      createBlock,
      validateBlock,
      proposeEffect,
      validatorIdentity
    )
  }
}
