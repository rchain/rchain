package coop.rchain.casper.blocks.proposer

import cats.effect.Concurrent
import cats.effect.concurrent.Deferred
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{Casper, _}
import coop.rchain.crypto.PrivateKey
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.{EventPublisher, Log, Stopwatch, Time}
import fs2.Stream
import coop.rchain.metrics.implicits._

class Proposer[F[_]: Concurrent: Log: Span](
    // base state on top of which block will be created
    getCasperSnapshot: Casper[F] => F[CasperSnapshot[F]],
    // propose constraint checkers
    checkActiveValidator: (
        CasperSnapshot[F],
        ValidatorIdentity
    ) => CheckProposeConstraintsResult,
    checkEnoughBaseStake: (BlockMessage, CasperSnapshot[F]) => F[CheckProposeConstraintsResult],
    checkFinalizedHeight: (BlockMessage, CasperSnapshot[F]) => F[CheckProposeConstraintsResult],
    createBlock: (
        CasperSnapshot[F],
        ValidatorIdentity
    ) => F[BlockCreatorResult],
    validateBlock: (Casper[F], CasperSnapshot[F], BlockMessage) => F[ValidBlockProcessing],
    proposeEffect: (Casper[F], BlockMessage) => F[Unit],
    validator: ValidatorIdentity
) {

  implicit val RuntimeMetricsSource: Source = Metrics.Source(CasperMetricsSource, "proposer")
  // This is the whole logic of propose
  private def doPropose(
      s: CasperSnapshot[F],
      casper: Casper[F]
  ): F[(ProposeResult, Option[BlockMessage])] =
    Span[F].traceI("do-propose") {
      for {
        // TODO this genesis should not be here, but required for sync constraint code. Remove
        genesis <- casper.getApprovedBlock
        // check if node is allowed to propose a block
        chk <- checkProposeConstraints(genesis, s)
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
                          validateBlock(casper, s, b).flatMap {
                            case Right(v) =>
                              proposeEffect(casper, b) >>
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
      genesis: BlockMessage,
      s: CasperSnapshot[F]
  ): F[CheckProposeConstraintsResult] = {
    val s1   = Stream[F, CheckProposeConstraintsResult](checkActiveValidator(s, validator))
    val s2   = Stream.eval[F, CheckProposeConstraintsResult](checkEnoughBaseStake(genesis, s))
    val s3   = Stream.eval[F, CheckProposeConstraintsResult](checkFinalizedHeight(genesis, s))
    val work = Stream(s1, s2, s3)
    work
      .parJoin(3)
      .compile
      .toList
      // pick some result that is not Success, or return Success
      .map(_.find(_ != CheckProposeConstraintsSuccess).getOrElse(CheckProposeConstraintsSuccess))
  }

  def propose(
      c: Casper[F],
      proposeIdDef: Deferred[F, Option[Int]]
  ): F[(ProposeResult, Option[BlockMessage])] =
    for {
      // get snapshot to serve as a base for propose
      s       <- Stopwatch.time(Log[F].info(_))(s"getCasperSnapshot")(getCasperSnapshot(c))
      nextSeq = ((s.maxSeqNums.getOrElse(ByteString.copyFrom(validator.publicKey.bytes), 0)) + 1)
      // resolve proposeID for the caller
      _ <- proposeIdDef.complete(nextSeq.some)
      // propose
      r <- doPropose(s, c)
    } yield r
}

object Proposer {
  // format: off
  def apply[F[_]
    /* Execution */   : Concurrent: Time
    /* Casper */      : Estimator: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker
    /* Storage */     : BlockStore: BlockDagStorage: LastFinalizedStorage: DeployStorage
    /* Diagnostics */ : Log: Span: Metrics: EventPublisher
    /* Comm */        : CommUtil: BlockRetriever
  ] // format: on
  (
      validatorIdentity: ValidatorIdentity,
      dummyDeployOpt: Option[(PrivateKey, String)] = None
  )(implicit runtimeManager: RuntimeManager[F]): Proposer[F] = {
    val getCasperSnapshotSnapshot = (c: Casper[F]) => c.getSnapshot()

    val createBlock = (s: CasperSnapshot[F], validatorIdentity: ValidatorIdentity) =>
      BlockCreator.create(s, validatorIdentity, dummyDeployOpt)

    val validateBlock = (casper: Casper[F], s: CasperSnapshot[F], b: BlockMessage) =>
      casper.validate(b, s)

    val checkValidatorIsActive = (s: CasperSnapshot[F], validator: ValidatorIdentity) =>
      if (s.onChainState.activeValidators.contains(ByteString.copyFrom(validator.publicKey.bytes)))
        CheckProposeConstraintsSuccess
      else
        NotBonded

    val checkEnoughBaseStake = (genesis: BlockMessage, s: CasperSnapshot[F]) =>
      SynchronyConstraintChecker[F].check(
        s,
        runtimeManager,
        genesis,
        validatorIdentity
      )

    val checkLastFinalizedHeightConstraint = (genesis: BlockMessage, s: CasperSnapshot[F]) =>
      LastFinalizedHeightConstraintChecker[F].check(
        s,
        genesis: BlockMessage,
        validatorIdentity
      )

    val proposeEffect = (c: Casper[F], b: BlockMessage) =>
      // store block
      BlockStore[F].put(b) >>
        // save changes to Casper
        c.handleValidBlock(b) >>
        // inform block retriever about block
        BlockRetriever[F].ackInCasper(b.blockHash) >>
        // broadcast hash to peers
        CommUtil[F].sendBlockHash(b.blockHash, b.sender) >>
        // Publish event
        EventPublisher[F].publish(MultiParentCasperImpl.createdEvent(b))

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
