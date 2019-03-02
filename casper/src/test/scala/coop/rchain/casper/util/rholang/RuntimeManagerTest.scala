package coop.rchain.casper.util.rholang

import cats.Id
import cats.effect.Resource
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.Resources._
import coop.rchain.catscontrib.TestOutlaws._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.{accounting, Interpreter}
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RuntimeManagerTest extends FlatSpec with Matchers {
  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("casper-runtime-manager-test")

  "computeState" should "capture rholang errors" in {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    val deploy     = ProtoUtil.sourceDeployNow(badRholang)
    val (_, Seq(result)) =
      runtimeManager
        .use(mgr => mgr.computeState(mgr.emptyStateHash, deploy :: Nil))
        .runSyncUnsafe(10.seconds)

    result.status.isFailed should be(true)
  }

  it should "capture rholang parsing errors and charge for parsing" in {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!("hi") """
    val deploy     = ProtoUtil.sourceDeployNow(badRholang)
    val (_, Seq(result)) =
      runtimeManager
        .use(mgr => mgr.computeState(mgr.emptyStateHash, deploy :: Nil))
        .runSyncUnsafe(10.seconds)

    result.status.isFailed should be(true)
    result.cost.cost shouldEqual (accounting.parsingCost(badRholang).value)
  }

  it should "charge for parsing and execution" in {
    val correctRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!(2) }"""
    val deploy         = ProtoUtil.sourceDeployNow(correctRholang)

    implicit val log: Log[Task]            = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    (for {
      reductionCost <- mkRuntime("casper-runtime")
                        .use { runtime =>
                          implicit val rand: Blake2b512Random = Blake2b512Random(
                            DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
                          )
                          val initialPhlo = Cost(accounting.MAX_VALUE)
                          for {
                            _             <- runtime.reducer.setPhlo(initialPhlo)
                            term          <- Interpreter[Task].buildNormalizedTerm(deploy.term)
                            _             <- runtime.reducer.inj(term)
                            phlosLeft     <- runtime.reducer.phlo
                            reductionCost = initialPhlo - phlosLeft
                          } yield (reductionCost)
                        }
      result <- mkRuntimeManager("casper-runtime-manager")
                 .use {
                   case runtimeManager =>
                     for {
                       state <- runtimeManager.computeState(
                                 runtimeManager.emptyStateHash,
                                 deploy :: Nil
                               )
                       result = state._2.head
                     } yield (result)
                 }
      _           = result.status.isFailed should be(false)
      parsingCost = accounting.parsingCost(correctRholang)
    } yield (result.cost.cost shouldEqual ((parsingCost + reductionCost).value)))
      .runSyncUnsafe(10.seconds)
  }

  "captureResult" should "return the value at the specified channel after a rholang computation" in {
    val purseValue     = "37"
    val captureChannel = "__PURSEVALUE__"
    val deployData = ProtoUtil.sourceDeploy(
      s"""new rl(`rho:registry:lookup`), NonNegativeNumberCh in {
         |  rl!(`rho:id:nd74ztexkao5awjhj95e3octkza7tydwiy7euthnyrt5ihgi9rj495`, *NonNegativeNumberCh) |
         |  for(@(_, NonNegativeNumber) <- NonNegativeNumberCh) {
         |    @NonNegativeNumber!($purseValue, "nn")
         |  }
         |}""".stripMargin,
      System.currentTimeMillis(),
      accounting.MAX_VALUE
    )

    val result =
      runtimeManager
        .use { mgr =>
          mgr
            .computeState(mgr.emptyStateHash, Seq(StandardDeploys.nonNegativeNumber, deployData))
            .flatMap { result =>
              val hash = result._1
              mgr
                .captureResults(
                  hash,
                  ProtoUtil.sourceDeploy(
                    s""" for(nn <- @"nn"){ nn!("value", "$captureChannel") } """,
                    0L,
                    accounting.MAX_VALUE
                  ),
                  captureChannel
                )
            }
        }
        .runSyncUnsafe(10.seconds)

    result.size should be(1)
    result.head should be(InterpreterUtil.mkTerm(purseValue).right.get)
  }

  it should "handle multiple results and no results appropriately" in {
    val n    = 8
    val code = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term = ProtoUtil.sourceDeploy(code, 0L, accounting.MAX_VALUE)
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
    (1 to n).forall(i => manyResults.contains(InterpreterUtil.mkTerm(i.toString).right.get)) should be(
      true
    )
  }

  "emptyStateHash" should "not remember previous hot store state" in {
    implicit val timeEff: LogicalTime[Id] = new LogicalTime[Id]

    import cats.implicits._

    val terms = ProtoUtil.basicDeployData[Id](0) :: Nil

    def run =
      runtimeManager
        .use { m =>
          val hash = m.emptyStateHash
          m.computeState(hash, terms)
            .map(_ => hash)
        }

    val hash1, hash2 = run.product(run).runSyncUnsafe(10.seconds)

    hash1 should be(hash2)
  }

  "computeState" should "charge deploys separately" in {
    val terms = List(
      """for(@x <- @"w") { @"z"!("Got x") }""",
      """for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!(10) }"""
    )

    def deployCost(p: Seq[InternalProcessedDeploy]): Long = p.map(_.cost.cost).sum
    val deploy = terms.map(
      t =>
        ProtoUtil.sourceDeploy(
          t,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
    )
    val (_, firstDeploy) =
      runtimeManager
        .use(mgr => mgr.computeState(mgr.emptyStateHash, deploy.head :: Nil))
        .runSyncUnsafe(10.seconds)

    val (_, secondDeploy) =
      runtimeManager
        .use(mgr => mgr.computeState(mgr.emptyStateHash, deploy.drop(1).head :: Nil))
        .runSyncUnsafe(10.seconds)

    val (_, compoundDeploy) =
      runtimeManager
        .use(mgr => mgr.computeState(mgr.emptyStateHash, deploy))
        .runSyncUnsafe(10.seconds)

    assert(firstDeploy.size == 1)
    val firstDeployCost = deployCost(firstDeploy)
    assert(secondDeploy.size == 1)
    val secondDeployCost = deployCost(secondDeploy)
    assert(compoundDeploy.size == 2)
    val compoundDeployCost = deployCost(compoundDeploy)
    assert(firstDeployCost < compoundDeployCost)
    assert(secondDeployCost < compoundDeployCost)
    assert(
      firstDeployCost == deployCost(
        compoundDeploy.find(_.deploy == firstDeploy.head.deploy).toVector
      )
    )
    assert(
      secondDeployCost == deployCost(
        compoundDeploy.find(_.deploy == secondDeploy.head.deploy).toVector
      )
    )
    assert((firstDeployCost + secondDeployCost) == compoundDeployCost)
  }
}
