package coop.rchain.casper.addblock

import cats.Applicative
import cats.effect.concurrent.Deferred
import cats.syntax.all._
import coop.rchain.casper._
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.helper.BlockDagStorageFixture
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.GenesisBuilder.randomValidatorSks
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProposerSpec extends AnyFlatSpec with Matchers with BlockDagStorageFixture {

  def getLatestSeqNumber[F[_]: Applicative](sender: Validator): F[Long] = (-1L).pure[F]

  def alwaysNotActiveF[F[_]: Applicative]: ValidatorIdentity => F[Boolean] =
    (_: ValidatorIdentity) => false.pure[F]

  def alwaysActiveF[F[_]: Applicative]: ValidatorIdentity => F[Boolean] =
    (_: ValidatorIdentity) => true.pure[F]

  def alwaysSuccesfullValidation[F[_]: Applicative] =
    (_: BlockMessage) => BlockStatus.valid.asRight[BlockError].pure[F]

  def alwaysUnsuccesfullValidation[F[_]: Applicative] =
    (_: BlockMessage) => BlockStatus.invalidSequenceNumber.asLeft[ValidBlock].pure[F]

  // var to estimate result of executing of propose effect
  var proposeEffectVar: Int = 0

  def proposeEffect[F[_]: Applicative](v: Int) =
    (_: BlockMessage) => (proposeEffectVar = v).pure[F]

  def createBlockF[F[_]: Applicative] =
    (_: ValidatorIdentity) => BlockCreatorResult.created(getRandomBlock()).pure[F]

  val dummyValidatorIdentity = ValidatorIdentity(randomValidatorSks(1))

  /** implicits for creating Proposer instance  */
  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val spanEff: Span[Task] = NoopSpan[Task]
  implicit val metrics             = new MetricsNOP[Task]()

  it should "reject to propose if proposer is not active validator" in effectTest {
    val p = new Proposer[Task](
      checkActiveValidator = alwaysNotActiveF[Task],
      // other params are permissive
      getLatestSeqNumber = getLatestSeqNumber[Task],
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

  it should "shut down the node if block created is not successfully replayed" in {
    an[Throwable] should be thrownBy {
      val p = new Proposer[Task](
        validateBlock = alwaysUnsuccesfullValidation[Task],
        // other params are permissive
        checkActiveValidator = alwaysActiveF[Task],
        getLatestSeqNumber = getLatestSeqNumber[Task],
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
      checkActiveValidator = alwaysActiveF[Task],
      getLatestSeqNumber = getLatestSeqNumber[Task],
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
