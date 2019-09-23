package coop.rchain.casper.util.rholang

import cats.effect.Resource
import cats.implicits._
import cats.{Functor, Id}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder, ProtoUtil}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{accounting, ParBuilderUtil}
import coop.rchain.shared.{Log, Time}
import coop.rchain.{metrics, rholang}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RuntimeManagerTest extends FlatSpec with Matchers {

  implicit val timeF: Time[Task]         = new LogicalTime[Task]
  implicit val log: Log[Task]            = Log.log[Task]
  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock

  val runtimeManager: Resource[Task, RuntimeManager[Task]] = for {
    dirs <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    rm   <- Resources.mkRuntimeManagerAt[Task](dirs.rspaceDir)()
  } yield rm

  val runtimeAndManager: Resource[Task, (interpreter.Runtime[Task], RuntimeManager[Task])] = for {
    dirs <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    r    <- rholang.Resources.mkRuntimeAt[Task](dirs.rspaceDir)()
    rm   <- Resource.liftF[Task, RuntimeManager[Task]](RuntimeManager.fromRuntime[Task](r))
  } yield (r, rm)

  private def computeState[F[_]: Functor](
      runtimeManager: RuntimeManager[F],
      deploy: DeployData
  ): F[(StateHash, InternalProcessedDeploy)] =
    for {
      res <- runtimeManager.computeState(ProtoUtil.tuplespace(genesis))(
              deploy :: Nil,
              BlockData(deploy.timestamp, 0),
              Map.empty[BlockHash, Validator]
            )
      (hash, Seq(result)) = res
    } yield (hash, result)

  "computeState" should "capture rholang errors" in effectTest {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    for {
      deploy <- ConstructDeploy.sourceDeployNowF(badRholang)
      result <- runtimeManager.use(computeState(_, deploy))
      _      = result._2.status.isFailed should be(true)
    } yield ()
  }

  it should "capture rholang parsing errors and charge for parsing" in effectTest {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!("hi") """
    for {
      deploy <- ConstructDeploy.sourceDeployNowF(badRholang)
      result <- runtimeManager.use(computeState(_, deploy))
      _      = result._2.status.isFailed should be(true)
      _      = result._2.cost.cost shouldEqual accounting.parsingCost(badRholang).value
    } yield ()
  }

  it should "charge for parsing and execution" in effectTest {
    val correctRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!(2) }"""

    runtimeAndManager
      .use {
        case (runtime, runtimeManager) => {
          implicit val rand: Blake2b512Random = Blake2b512Random(128)
          val initialPhlo                     = Cost.UNSAFE_MAX
          for {
            deploy <- ConstructDeploy.sourceDeployNowF(correctRholang)

            _             <- runtime.cost.set(initialPhlo)
            term          <- ParBuilderUtil.buildNormalizedTerm[Task](deploy.term)
            _             <- runtime.reducer.inj(term)
            phlosLeft     <- runtime.cost.get
            reductionCost = initialPhlo - phlosLeft

            parsingCost = accounting.parsingCost(correctRholang)

            result <- computeState(runtimeManager, deploy)

            _ = result._2.cost.cost shouldEqual (reductionCost + parsingCost).value
          } yield ()
        }
      }
  }

  "captureResult" should "return the value at the specified channel after a rholang computation" in effectTest {
    val purseValue     = "37"
    val captureChannel = "__PURSEVALUE__"

    runtimeManager.use { mgr =>
      for {
        deploy0 <- ConstructDeploy.sourceDeployNowF(
                    s"""
                      |new rl(`rho:registry:lookup`), NonNegativeNumberCh in {
                      |  rl!(`rho:lang:nonNegativeNumber`, *NonNegativeNumberCh) |
                      |  for(@(_, NonNegativeNumber) <- NonNegativeNumberCh) {
                      |    @NonNegativeNumber!($purseValue, "nn")
                      |  }
                      |}""".stripMargin
                  )
        result0 <- computeState(mgr, deploy0)
        hash    = result0._1
        deploy1 <- ConstructDeploy.sourceDeployNowF(
                    s""" for(nn <- @"nn"){ nn!("value", "$captureChannel") } """
                  )
        result1 <- mgr.captureResults(hash, deploy1, captureChannel)

        _ = result1.size should be(1)
        _ = result1.head should be(ParBuilderUtil.mkTerm(purseValue).right.get)
      } yield ()
    }
  }

  it should "handle multiple results and no results appropriately" in {
    val n    = 8
    val code = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term = ConstructDeploy.sourceDeploy(code, timestamp = 0)
    val manyResults =
      runtimeManager
        .use(mgr => mgr.captureResults(mgr.emptyStateHash, term))
        .runSyncUnsafe(10.seconds)
    val noResults =
      runtimeManager
        .use(mgr => mgr.captureResults(mgr.emptyStateHash, term, "differentName"))
        .runSyncUnsafe(10.seconds)

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(ParBuilderUtil.mkTerm(i.toString).right.get)) should be(
      true
    )
  }

  "emptyStateHash" should "not remember previous hot store state" in effectTest {
    implicit val timeEff: LogicalTime[Id] = new LogicalTime[Id]

    val term = ConstructDeploy.basicDeployData[Id](0)

    def run: Task[StateHash] =
      runtimeManager
        .use { m =>
          val hash = m.emptyStateHash
          computeState(m, term)
            .map(_ => hash)
        }

    for {
      res            <- run.product(run)
      (hash1, hash2) = res
      _              = hash1 should be(hash2)
    } yield ()
  }

  "computeState" should "charge deploys separately" in effectTest {

    def deployCost(p: Seq[InternalProcessedDeploy]): Long = p.map(_.cost.cost).sum

    runtimeManager.use { mgr =>
      for {
        deploy0 <- ConstructDeploy.sourceDeployNowF(""" for(@x <- @"w") { @"z"!("Got x") } """)
        deploy1 <- ConstructDeploy.sourceDeployNowF(
                    """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!(10) } """
                  )
        time          <- timeF.currentMillis
        genPostState  = genesis.body.state.postStateHash
        blockData     = BlockData(time, 0L)
        invalidBlocks = Map.empty[BlockHash, Validator]
        firstDeploy <- mgr
                        .computeState(genPostState)(deploy0 :: Nil, blockData, invalidBlocks)
                        .map(_._2)
        secondDeploy <- mgr
                         .computeState(genPostState)(deploy1 :: Nil, blockData, invalidBlocks)
                         .map(_._2)
        compoundDeploy <- mgr
                           .computeState(genPostState)(
                             deploy0 :: deploy1 :: Nil,
                             blockData,
                             invalidBlocks
                           )
                           .map(_._2)
        _                  = assert(firstDeploy.size == 1)
        _                  = assert(secondDeploy.size == 1)
        _                  = assert(compoundDeploy.size == 2)
        firstDeployCost    = deployCost(firstDeploy)
        secondDeployCost   = deployCost(secondDeploy)
        compoundDeployCost = deployCost(compoundDeploy)
        _                  = assert(firstDeployCost < compoundDeployCost)
        _                  = assert(secondDeployCost < compoundDeployCost)
        _ = assert(
          firstDeployCost == deployCost(
            compoundDeploy.find(_.deploy == firstDeploy.head.deploy).toVector
          )
        )
        _ = assert(
          secondDeployCost == deployCost(
            compoundDeploy.find(_.deploy == secondDeploy.head.deploy).toVector
          )
        )
        _ = assert((firstDeployCost + secondDeployCost) == compoundDeployCost)
      } yield ()
    }
  }
}
