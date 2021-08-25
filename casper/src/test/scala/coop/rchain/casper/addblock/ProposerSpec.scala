package coop.rchain.casper.addblock

import cats.Applicative
import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.all._
import coop.rchain.casper._
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.helper.{BlockDagStorageFixture, NoOpsCasperEffect}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.GenesisBuilder.defaultValidatorSks
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class ProposerSpec extends FlatSpec with Matchers with BlockDagStorageFixture {

  /** declarations of input functions for proposer */
  def getCasperSnapshotF[F[_]: Applicative]: Casper[F] => F[CasperSnapshot[F]] =
    _ => Resources.mkDummyCasperSnapshot

  def alwaysNotActiveF[F[_]: Applicative]
      : (CasperSnapshot[F], ValidatorIdentity) => CheckProposeConstraintsResult =
    (_: CasperSnapshot[F], _: ValidatorIdentity) => CheckProposeConstraintsResult.notBonded

  def alwaysActiveF[F[_]: Applicative]
      : (CasperSnapshot[F], ValidatorIdentity) => CheckProposeConstraintsResult =
    (_: CasperSnapshot[F], _: ValidatorIdentity) => CheckProposeConstraintsResult.success

  def alwaysNotEnoughBlocksF[F[_]: Applicative]
      : (BlockMessage, CasperSnapshot[F]) => F[CheckProposeConstraintsResult] =
    (_: BlockMessage, _: CasperSnapshot[F]) =>
      CheckProposeConstraintsResult.notEnoughNewBlock.pure[F]

  def alwaysTooFarAheadF[F[_]: Applicative]
      : (BlockMessage, CasperSnapshot[F]) => F[CheckProposeConstraintsResult] =
    (_: BlockMessage, _: CasperSnapshot[F]) =>
      CheckProposeConstraintsResult.tooFarAheadOfLastFinalized.pure[F]

  def okProposeConstraint[F[_]: Applicative]
      : (BlockMessage, CasperSnapshot[F]) => F[CheckProposeConstraintsResult] =
    (_: BlockMessage, _: CasperSnapshot[F]) => CheckProposeConstraintsResult.success.pure[F]

  def alwaysSuccesfullValidation[F[_]: Applicative] =
    (_: Casper[F], _: CasperSnapshot[F], _: BlockMessage) =>
      BlockStatus.valid.asRight[BlockError].pure[F]

  def alwaysUnsuccesfullValidation[F[_]: Applicative] =
    (_: Casper[F], _: CasperSnapshot[F], _: BlockMessage) =>
      BlockStatus.invalidFormat.asLeft[ValidBlock].pure[F]

  // var to estimate result of executing of propose effect
  var proposeEffectVar: Int = 0

  def proposeEffect[F[_]: Applicative](v: Int) =
    (_: Casper[F], _: BlockMessage) => (proposeEffectVar = v).pure[F]

  def createBlockF[F[_]: Applicative] =
    (_: CasperSnapshot[F], _: ValidatorIdentity) =>
      BlockCreatorResult.created(getRandomBlock()).pure[F]

  val dummyValidatorIdentity = ValidatorIdentity(defaultValidatorSks(1))

  /** implicits for creating Proposer instance  */
  implicit val logEff: Log[Task]   = Log.log[Task]
  implicit val spanEff: Span[Task] = NoopSpan[Task]
  implicit val metrics             = new MetricsNOP[Task]()

  private val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager[Task]("block-query-response-api-test")

  it should "reject to propose if proposer is not active validator" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      {
        runtimeManagerResource
          .use { runtimeManager =>
            implicit val rm = runtimeManager
            val casper =
              NoOpsCasperEffect[Task](HashMap.empty[BlockHash, BlockMessage]).runSyncUnsafe()
            val p = new Proposer[Task](
              checkActiveValidator = alwaysNotActiveF,
              // other params are permissive
              checkEnoughBaseStake = okProposeConstraint,
              getCasperSnapshot = getCasperSnapshotF,
              checkFinalizedHeight = okProposeConstraint,
              createBlock = createBlockF,
              validateBlock = alwaysSuccesfullValidation,
              proposeEffect = proposeEffect(0),
              validator = dummyValidatorIdentity,
              loadDeploys = Set.empty[Signed[DeployData]].pure[Task]
            )

            for {
              d      <- Deferred[Task, ProposerResult]
              pr     <- p.propose(casper, false, d)
              (r, b) = pr
            } yield assert(r == ProposeResult.notBonded && b.isEmpty)
          }
      }
  }

  it should "reject to propose if synchrony constraint is not met" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      {
        runtimeManagerResource
          .use { runtimeManager =>
            implicit val rm = runtimeManager
            val casper =
              NoOpsCasperEffect[Task](HashMap.empty[BlockHash, BlockMessage]).runSyncUnsafe()

            val p = new Proposer[Task](
              checkEnoughBaseStake = alwaysNotEnoughBlocksF, // synchrony constraint is not met
              // other params are permissive
              getCasperSnapshot = getCasperSnapshotF,
              checkActiveValidator = alwaysActiveF,
              checkFinalizedHeight = okProposeConstraint,
              createBlock = createBlockF,
              validateBlock = alwaysSuccesfullValidation,
              proposeEffect = proposeEffect(0),
              validator = dummyValidatorIdentity,
              loadDeploys = Set.empty[Signed[DeployData]].pure[Task]
            )

            for {
              d      <- Deferred[Task, ProposerResult]
              pr     <- p.propose(casper, false, d)
              (r, b) = pr
            } yield assert(r == ProposeResult.notEnoughBlocks && b.isEmpty)
          }
      }
  }

  it should "reject to propose if last finalized height constraint is not met" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      {
        runtimeManagerResource
          .use { runtimeManager =>
            implicit val rm = runtimeManager
            val casper =
              NoOpsCasperEffect[Task](HashMap.empty[BlockHash, BlockMessage]).runSyncUnsafe()
            val p = new Proposer[Task](
              checkFinalizedHeight = alwaysTooFarAheadF,
              // other params are permissive
              checkActiveValidator = alwaysActiveF,
              checkEnoughBaseStake = okProposeConstraint,
              getCasperSnapshot = getCasperSnapshotF,
              createBlock = createBlockF,
              validateBlock = alwaysSuccesfullValidation,
              proposeEffect = proposeEffect(0),
              validator = dummyValidatorIdentity,
              loadDeploys = Set.empty[Signed[DeployData]].pure[Task]
            )
            for {
              d      <- Deferred[Task, ProposerResult]
              pr     <- p.propose(casper, false, d)
              (r, b) = pr
            } yield assert(r == ProposeResult.tooFarAheadOfLastFinalized && b.isEmpty)
          }
      }
  }
  it should "shut down the node if block created is not successfully replayed" in {
    an[Throwable] should be thrownBy {
      withStorage { implicit blockStore => implicit blockDagStorage =>
        {
          runtimeManagerResource
            .use { runtimeManager =>
              implicit val rm = runtimeManager
              val casper =
                NoOpsCasperEffect[Task](HashMap.empty[BlockHash, BlockMessage]).runSyncUnsafe()
              val p = new Proposer[Task](
                validateBlock = alwaysUnsuccesfullValidation,
                // other params are permissive
                checkFinalizedHeight = okProposeConstraint,
                checkActiveValidator = alwaysActiveF,
                checkEnoughBaseStake = okProposeConstraint,
                getCasperSnapshot = getCasperSnapshotF,
                createBlock = createBlockF,
                proposeEffect = proposeEffect(0),
                validator = dummyValidatorIdentity,
                loadDeploys = Set.empty[Signed[DeployData]].pure[Task]
              )

              for {
                d <- Deferred[Task, ProposerResult]
                _ <- p.propose(casper, false, d)
              } yield ()
            }
        }
      }
    }
  }

  it should "execute propose effects if block created successfully replayed" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      {
        runtimeManagerResource
          .use { runtimeManager =>
            implicit val rm = runtimeManager
            val casper =
              NoOpsCasperEffect[Task](HashMap.empty[BlockHash, BlockMessage]).runSyncUnsafe()
            val p = new Proposer[Task](
              validateBlock = alwaysSuccesfullValidation,
              checkFinalizedHeight = okProposeConstraint,
              checkActiveValidator = alwaysActiveF,
              checkEnoughBaseStake = okProposeConstraint,
              getCasperSnapshot = getCasperSnapshotF,
              createBlock = createBlockF,
              proposeEffect = proposeEffect(10),
              validator = dummyValidatorIdentity,
              loadDeploys = Set.empty[Signed[DeployData]].pure[Task]
            )

            for {
              d      <- Deferred[Task, ProposerResult]
              pr     <- p.propose(casper, false, d)
              (r, b) = pr
            } yield assert(
              r == ProposeResult.success(BlockStatus.valid) && b.nonEmpty && proposeEffectVar == 10
            )
          }
      }
  }
}
