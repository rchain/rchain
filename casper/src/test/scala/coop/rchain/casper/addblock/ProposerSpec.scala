package coop.rchain.casper.addblock

import cats.Applicative
import cats.effect.concurrent.Deferred
import cats.syntax.all._
import coop.rchain.casper._
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.helper.BlockDagStorageFixture
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.Resources
import coop.rchain.casper.util.GenesisBuilder.defaultValidatorSks
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class ProposerSpec extends FlatSpec with Matchers with BlockDagStorageFixture {

  /** declarations of input functions for proposer */
  def getCasperSnapshotF[F[_]: Applicative]: F[CasperSnapshot] =
    Resources.mkDummyCasperSnapshot

  def alwaysNotActiveF[F[_]: Applicative]
      : (CasperSnapshot, ValidatorIdentity) => CheckProposeConstraintsResult =
    (_: CasperSnapshot, _: ValidatorIdentity) => CheckProposeConstraintsResult.notBonded

  def alwaysActiveF[F[_]: Applicative]
      : (CasperSnapshot, ValidatorIdentity) => CheckProposeConstraintsResult =
    (_: CasperSnapshot, _: ValidatorIdentity) => CheckProposeConstraintsResult.success

  def alwaysNotEnoughBlocksF[F[_]: Applicative]
      : CasperSnapshot => F[CheckProposeConstraintsResult] =
    (_: CasperSnapshot) => CheckProposeConstraintsResult.notEnoughNewBlock.pure[F]

  def alwaysTooFarAheadF[F[_]: Applicative]: CasperSnapshot => F[CheckProposeConstraintsResult] =
    (_: CasperSnapshot) => CheckProposeConstraintsResult.tooFarAheadOfLastFinalized.pure[F]

  def okProposeConstraint[F[_]: Applicative]: CasperSnapshot => F[CheckProposeConstraintsResult] =
    (_: CasperSnapshot) => CheckProposeConstraintsResult.success.pure[F]

  def alwaysSuccesfullValidation[F[_]: Applicative] =
    (_: CasperSnapshot, _: BlockMessage) => BlockStatus.valid.asRight[BlockError].pure[F]

  def alwaysUnsuccesfullValidation[F[_]: Applicative] =
    (_: CasperSnapshot, _: BlockMessage) => BlockStatus.invalidFormat.asLeft[ValidBlock].pure[F]

  // var to estimate result of executing of propose effect
  var proposeEffectVar: Int = 0

  def proposeEffect[F[_]: Applicative](v: Int) =
    (_: BlockMessage) => (proposeEffectVar = v).pure[F]

  def createBlockF[F[_]: Applicative] =
    (_: CasperSnapshot, _: ValidatorIdentity) =>
      BlockCreatorResult.created(getRandomBlock()).pure[F]

  val dummyValidatorIdentity = ValidatorIdentity(defaultValidatorSks(1))

  /** implicits for creating Proposer instance  */
  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val spanEff: Span[Task] = NoopSpan[Task]
  implicit val metrics             = new MetricsNOP[Task]()

  it should "reject to propose if proposer is not active validator" in effectTest {
    val p = new Proposer[Task](
      checkActiveValidator = alwaysNotActiveF[Task],
      // other params are permissive
      checkEnoughBaseStake = okProposeConstraint[Task],
      getCasperSnapshot = getCasperSnapshotF[Task],
      checkFinalizedHeight = okProposeConstraint[Task],
      createBlock = createBlockF[Task],
      validateBlock = alwaysSuccesfullValidation[Task],
      proposeEffect = proposeEffect[Task](0),
      validator = dummyValidatorIdentity
    )

    for {
      d      <- Deferred[Task, ProposerResult]
      pr     <- p.propose(false, d)
      (r, b) = pr
    } yield assert(r == ProposeResult.notBonded && b.isEmpty)
  }

  it should "reject to propose if synchrony constraint is not met" in effectTest {
    val p = new Proposer[Task](
      checkEnoughBaseStake = alwaysNotEnoughBlocksF[Task], // synchrony constraint is not met
      // other params are permissive
      getCasperSnapshot = getCasperSnapshotF[Task],
      checkActiveValidator = alwaysActiveF[Task],
      checkFinalizedHeight = okProposeConstraint[Task],
      createBlock = createBlockF[Task],
      validateBlock = alwaysSuccesfullValidation[Task],
      proposeEffect = proposeEffect[Task](0),
      validator = dummyValidatorIdentity
    )

    for {
      d      <- Deferred[Task, ProposerResult]
      pr     <- p.propose(false, d)
      (r, b) = pr
    } yield assert(r == ProposeResult.notEnoughBlocks && b.isEmpty)
  }

  it should "reject to propose if last finalized height constraint is not met" in effectTest {
    val p = new Proposer[Task](
      checkFinalizedHeight = alwaysTooFarAheadF[Task],
      // other params are permissive
      checkActiveValidator = alwaysActiveF[Task],
      checkEnoughBaseStake = okProposeConstraint[Task],
      getCasperSnapshot = getCasperSnapshotF[Task],
      createBlock = createBlockF[Task],
      validateBlock = alwaysSuccesfullValidation[Task],
      proposeEffect = proposeEffect[Task](0),
      validator = dummyValidatorIdentity
    )
    for {
      d      <- Deferred[Task, ProposerResult]
      pr     <- p.propose(false, d)
      (r, b) = pr
    } yield assert(r == ProposeResult.tooFarAheadOfLastFinalized && b.isEmpty)
  }

  it should "shut down the node if block created is not successfully replayed" in {
    an[Throwable] should be thrownBy {
      val p = new Proposer[Task](
        validateBlock = alwaysUnsuccesfullValidation[Task],
        // other params are permissive
        checkFinalizedHeight = okProposeConstraint[Task],
        checkActiveValidator = alwaysActiveF[Task],
        checkEnoughBaseStake = okProposeConstraint[Task],
        getCasperSnapshot = getCasperSnapshotF[Task],
        createBlock = createBlockF[Task],
        proposeEffect = proposeEffect[Task](0),
        validator = dummyValidatorIdentity
      )

      (for {
        d <- Deferred[Task, ProposerResult]
        _ <- p.propose(false, d)
      } yield ()).runSyncUnsafe()
    }
  }

  it should "execute propose effects if block created successfully replayed" in effectTest {
    val p = new Proposer[Task](
      validateBlock = alwaysSuccesfullValidation[Task],
      checkFinalizedHeight = okProposeConstraint[Task],
      checkActiveValidator = alwaysActiveF[Task],
      checkEnoughBaseStake = okProposeConstraint[Task],
      getCasperSnapshot = getCasperSnapshotF[Task],
      createBlock = createBlockF[Task],
      proposeEffect = proposeEffect[Task](10),
      validator = dummyValidatorIdentity
    )

    for {
      d      <- Deferred[Task, ProposerResult]
      pr     <- p.propose(false, d)
      (r, b) = pr
    } yield assert(
      r == ProposeResult.success(BlockStatus.valid) && b.nonEmpty && proposeEffectVar == 10
    )
  }
}
