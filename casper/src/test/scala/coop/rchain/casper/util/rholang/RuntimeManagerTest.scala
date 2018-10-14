package coop.rchain.casper.util.rholang

import java.nio.file.Files

import coop.rchain.casper.protocol.PaymentDeploy
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Var.VarInstance.Wildcard
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models.{EVar, Par, Send}
import coop.rchain.rholang.interpreter.accounting.Chargeable
import coop.rchain.rholang.interpreter.{Runtime, accounting}
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.shared.StoreType
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class RuntimeManagerTest extends FlatSpec with Matchers {
  val storageSize      = 1024L * 1024
  val storageDirectory = Files.createTempDirectory("casper-runtime-manager-test")
  val activeRuntime    = Runtime.create(storageDirectory, storageSize, StoreType.LMDB)
  val runtimeManager   = RuntimeManager.fromRuntime(activeRuntime)

  "computeState" should "capture rholang errors" in {
    val badRholang       = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    val deploy           = ProtoUtil.termDeployNow(InterpreterUtil.mkTerm(badRholang).right.get)
    val (_, Seq(result)) = runtimeManager.computeState(runtimeManager.emptyStateHash, deploy :: Nil)

    result.status.isFailed should be(true)
  }

  "captureResult" should "return the value at the specified channel after a rholang computation" in {
    val purseValue     = "37"
    val captureChannel = "__PURSEVALUE__"
    val deploys = Seq(
      NonNegativeNumber.term,
      InterpreterUtil.mkTerm(s""" @"NonNegativeNumber"!($purseValue, "nn") """).right.get
    ).map(ProtoUtil.termDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))

    val (hash, _) = runtimeManager.computeState(runtimeManager.emptyStateHash, deploys)
    val result = runtimeManager.captureResults(
      hash,
      InterpreterUtil
        .mkTerm(s""" for(@nn <- @"nn"){ @(nn, "value")!("$captureChannel") } """)
        .right
        .get,
      captureChannel
    )

    result.size should be(1)
    result.head should be(InterpreterUtil.mkTerm(purseValue).right.get)
  }

  it should "handle multiple results and no results appropriately" in {
    val n    = 8
    val code = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term = InterpreterUtil.mkTerm(code).right.get
    val manyResults =
      runtimeManager.captureResults(runtimeManager.emptyStateHash, term, "__SCALA__")
    val noResults =
      runtimeManager.captureResults(runtimeManager.emptyStateHash, term, "differentName")

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(InterpreterUtil.mkTerm(i.toString).right.get)) should be(
      true
    )
  }

  "emptyStateHash" should "not remember previous hot store state" in {
    val testStorageDirectory = Files.createTempDirectory("casper-runtime-manager-test")

    val testRuntime1        = Runtime.create(testStorageDirectory, storageSize)
    val testRuntimeManager1 = RuntimeManager.fromRuntime(testRuntime1)
    val hash1               = testRuntimeManager1.emptyStateHash
    val deploy              = ProtoUtil.basicDeploy(0)
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
      runtimeManager.computeState(runtimeManager.emptyStateHash, deploy.head :: Nil)
    val (_, secondDeploy) =
      runtimeManager.computeState(runtimeManager.emptyStateHash, deploy.drop(1).head :: Nil)
    val (_, compoundDeploy) =
      runtimeManager.computeState(runtimeManager.emptyStateHash, deploy)
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

  "deployment" should "fail if payment deploy doesn't have payment code" in {
    val term   = InterpreterUtil.mkTerm("""new x in { x!(10) } """).right.get
    val deploy = ProtoUtil.termDeployNow(term).copy(paymentCode = None)
    val (_, Seq(processedDeploy)) =
      runtimeManager.computeState(runtimeManager.emptyStateHash, Seq(deploy))
    assert(processedDeploy.status.isFailed)
  }

  "deployment" should "fail if short leash deploy exceeds the short-leash phlo limit" in {
    import coop.rchain.models.rholang.implicits._
    val shortLeashCeilLimit: Long = RuntimeManager.SHORT_LEASH_COST_LIMIT.value
    val bigData: Par = {
      val char = "a"
      @tailrec
      def build(s: String): Par = {
        val par: Par = GString(s)
        if (Chargeable[Par].cost(par) > shortLeashCeilLimit) {
          par
        } else build(s ++ char)

      }
      build("")
    }
    val expensivePar: Par = Send(GPrivateBuilder(), Seq(bigData))
    val paymentDeploy     = PaymentDeploy().withCode(expensivePar)
    val deployPar: Par    = Send(GPrivateBuilder(), Seq(Par()))
    val deploy            = ProtoUtil.termDeployNow(deployPar).withPaymentCode(paymentDeploy)
    val (_, Seq(processedDeploy)) =
      runtimeManager.computeState(runtimeManager.emptyStateHash, Seq(deploy))
    // There's no way to tell what was the failure cause
    assert(processedDeploy.status.isFailed)
  }

  "deployment" should "fail if short leash deploy fails" in {
    import coop.rchain.models.rholang.implicits._
    // Top level wildcards are forbidden - should trigger the errors
    val invalidTerm: Par = Send(EVar(Wildcard(WildcardMsg())), Seq(GString("test")))
    val paymentDeploy    = PaymentDeploy().withCode(invalidTerm)
    val deploy =
      ProtoUtil.termDeployNow(Send(GPrivateBuilder(), Seq(Par()))).withPaymentCode(paymentDeploy)
    val (_, Seq(processedDeploy)) =
      runtimeManager.computeState(runtimeManager.emptyStateHash, Seq(deploy))
    // There's no way to tell what was the failure cause
    assert(processedDeploy.status.isFailed)
  }

  "deployment" should "charge for the payment deploy" in {
    import coop.rchain.models.rholang.implicits._
    import coop.rchain.rholang.interpreter.accounting._
    def sendWithCost(str: String): (Par, Cost) = {
      val data: Seq[Par] = Seq(GString(str))
      val channel: Par   = GPrivateBuilder()
      val send: Par      = Send(channel, data)
      val storageCost    = data.storageCost + channel.storageCost
      (send, storageCost)
    }
    val (paymentDeployPar, paymentDeployCost) = sendWithCost("Payment deploy")
    val paymentDeploy                         = PaymentDeploy().withCode(paymentDeployPar)
    val (deployTerm, deployCost)              = sendWithCost("Main deploy")
    val cost                                  = paymentDeployCost + deployCost + (SEND_EVAL_COST * 2)
    val deploy                                = ProtoUtil.termDeployNow(deployTerm).withPaymentCode(paymentDeploy)
    val (_, Seq(processedDeploy)) =
      runtimeManager.computeState(runtimeManager.emptyStateHash, Seq(deploy))
    assert(!processedDeploy.status.isFailed)
    assert(processedDeploy.cost.cost == cost.value)
  }
}
