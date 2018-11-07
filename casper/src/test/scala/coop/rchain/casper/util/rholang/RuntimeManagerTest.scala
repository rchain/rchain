package coop.rchain.casper.util.rholang

import java.nio.file.Files

import cats.Id
import cats.effect.Resource
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.Deploy
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.Capture._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.Resources.mkTempDir
import coop.rchain.rholang.interpreter.{accounting, Runtime}
import coop.rchain.shared.StoreType
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.effect.implicits.bracketTry
import scala.util.Try
import coop.rchain.catscontrib.Capture._
import scala.concurrent.duration._

class RuntimeManagerTest extends FlatSpec with Matchers {
  val storageSize = 1024L * 1024
  val runtimeManager: Resource[Try, RuntimeManager] =
    mkTempDir[Try]("casper-runtime-manager-test").flatMap(storageDirectory => {
      val activeRuntime = Runtime.create(storageDirectory, storageSize, StoreType.LMDB)
      Resource.pure(RuntimeManager.fromRuntime(activeRuntime))
    })

  "computeState" should "capture rholang errors" in {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    val deploy     = ProtoUtil.termDeployNow(InterpreterUtil.mkTerm(badRholang).right.get)
    val (_, Seq(result)) =
      runtimeManager
        .use(
          mgr =>
            Try {
              mgr
                .computeState(mgr.emptyStateHash, deploy :: Nil)
                .runSyncUnsafe(10.seconds)
            }
        )
        .get

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

    val result = runtimeManager.use { mgr =>
      Try {
        val (hash, _) =
          mgr
            .computeState(mgr.emptyStateHash, deploys)
            .runSyncUnsafe(10.seconds)

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
    }.get

    result.size should be(1)
    result.head should be(InterpreterUtil.mkTerm(purseValue).right.get)
  }

  it should "handle multiple results and no results appropriately" in {
    val n    = 8
    val code = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term = ProtoUtil.deployDataToDeploy(ProtoUtil.sourceDeploy(code, 0L, accounting.MAX_VALUE))
    val manyResults =
      runtimeManager
        .use(mgr => Try { mgr.captureResults(mgr.emptyStateHash, term, "__SCALA__") })
        .get
    val noResults =
      runtimeManager
        .use(mgr => Try { mgr.captureResults(mgr.emptyStateHash, term, "differentName") })
        .get

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(InterpreterUtil.mkTerm(i.toString).right.get)) should be(
      true
    )
  }

  "emptyStateHash" should "not remember previous hot store state" in {
    implicit val timeEff = new LogicalTime[Id]

    val testStorageDirectory = Files.createTempDirectory("casper-runtime-manager-test")

    val testRuntime1        = Runtime.create(testStorageDirectory, storageSize)
    val testRuntimeManager1 = RuntimeManager.fromRuntime(testRuntime1)
    val hash1               = testRuntimeManager1.emptyStateHash
    val deploy              = ProtoUtil.basicDeploy[Id](0)
    val _                   = testRuntimeManager1.computeState(hash1, deploy :: Nil)
    testRuntime1.close()

    val testRuntime2        = Runtime.create(testStorageDirectory, storageSize)
    val testRuntimeManager2 = RuntimeManager.fromRuntime(testRuntime2)
    val hash2               = testRuntimeManager2.emptyStateHash
    testRuntime2.close()

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
        .use(
          mgr =>
            Try {
              mgr.computeState(mgr.emptyStateHash, deploy.head :: Nil).runSyncUnsafe(10.seconds)
            }
        )
        .get
    val (_, secondDeploy) =
      runtimeManager
        .use(
          mgr =>
            Try {
              mgr
                .computeState(mgr.emptyStateHash, deploy.drop(1).head :: Nil)
                .runSyncUnsafe(10.seconds)
            }
        )
        .get
    val (_, compoundDeploy) =
      runtimeManager
        .use(mgr => Try { mgr.computeState(mgr.emptyStateHash, deploy).runSyncUnsafe(10.seconds) })
        .get
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
