package coop.rchain.casper.util.rholang

import cats.Id
import cats.effect.Resource
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.Deploy
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.catscontrib.Capture._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class RuntimeManagerTest extends FlatSpec with Matchers {
  private val runtimeManager: Resource[Task, RuntimeManager] =
    mkRuntimeManager("casper-runtime-manager-test")

  "computeState" should "capture rholang errors" in {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    val deploy     = ProtoUtil.termDeployNow(InterpreterUtil.mkTerm(badRholang).right.get)
    val (_, Seq(result)) =
      runtimeManager
        .use(mgr => mgr.computeState(mgr.emptyStateHash, deploy :: Nil))
        .runSyncUnsafe(10.seconds)

    result.status.isFailed should be(true)
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
    val deploys = Seq(
      StandardDeploys.nonNegativeNumber,
      Deploy(
        term = InterpreterUtil.mkTerm(deployData.term).toOption,
        raw = Some(deployData)
      )
    )

    val result =
      runtimeManager
        .use { mgr =>
          mgr
            .computeState(mgr.emptyStateHash, deploys)
            .map { result =>
              val hash = result._1
              mgr.captureResults(
                hash,
                ProtoUtil.deployDataToDeploy(
                  ProtoUtil.sourceDeploy(
                    s""" for(nn <- @"nn"){ nn!("value", "$captureChannel") } """,
                    0L,
                    accounting.MAX_VALUE
                  )
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
    val term = ProtoUtil.deployDataToDeploy(ProtoUtil.sourceDeploy(code, 0L, accounting.MAX_VALUE))
    val manyResults =
      runtimeManager
        .use(mgr => Task.delay { mgr.captureResults(mgr.emptyStateHash, term) })
        .runSyncUnsafe(10.seconds)
    val noResults =
      runtimeManager
        .use(mgr => Task.delay { mgr.captureResults(mgr.emptyStateHash, term, "differentName") })
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

    val terms = ProtoUtil.basicDeploy[Id](0) :: Nil

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
        ProtoUtil.termDeploy(
          InterpreterUtil.mkTerm(t).right.get,
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
