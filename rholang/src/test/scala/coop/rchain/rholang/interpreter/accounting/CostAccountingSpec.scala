package coop.rchain.rholang.interpreter.accounting

import java.nio.file.Files

import cats.effect.concurrent.Ref
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.accounting.utils._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.shared.StoreType
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class LegacyCostAccountingSpec extends FlatSpec with TripleEqualsSupport {

  behavior of "CostAccountingAlg"

  val defaultCost = Cost(33)

  def withCostAccAlg[T](
      initState: Cost = defaultCost
  )(f: CostAccounting[Task] => Task[T]): T = {
    val test = for {
      alg <- CostAccounting.of[Task](initState)
      res <- f(alg)
    } yield res
    Await.result(test.runToFuture, 5.seconds)
  }

  "get" should "return current cost account" in withCostAccAlg() { alg =>
    for {
      s <- alg.get()
    } yield assert(s === defaultCost)
  }

  "charge" should "modify underlying cost account" in withCostAccAlg() { alg =>
    val c = Cost(13)
    for {
      _ <- alg.charge(c)
      s <- alg.get()
    } yield assert(defaultCost - c === s)
  }

  def assertOutOfPhloError[A](test: Task[A]): Task[Assertion] =
    test.attempt.map(res => assert(res === Left(OutOfPhlogistonsError)))

  it should "fail when cost account goes below zero" in withCostAccAlg() { alg =>
    val cost = Cost(100)
    val test = for {
      _ <- alg.charge(cost)
      s <- alg.get()
    } yield s

    assertOutOfPhloError(test)
  }

  it should "fail when tries to charge on the cost account that is already negative" in withCostAccAlg(
    Cost(-100)
  ) { alg =>
    val cost = Cost(1)
    val test = for {
      _ <- alg.charge(cost)
      s <- alg.get()
    } yield s

    assertOutOfPhloError(test)
  }

}

class CostAccountingSpec extends FlatSpec with Matchers {

  private[this] def evaluateWithCostLog(
      initialPhlo: Long,
      contract: String
  ) = {
    implicit val errorLog = new ErrorLog[Task]()
    implicit val costAlg: CostAccounting[Task] =
      CostAccounting.unsafe[Task](Cost(initialPhlo))
    implicit val costL             = costLog[Task]
    implicit val cost: _cost[Task] = loggingCost(costAlg, costL)
    val dbDir                      = Files.createTempDirectory("cost-accounting-spec-")
    val size                       = 1024L * 1024 * 1024
    implicit val logF: Log[Task]   = new Log.NOPLog[Task]

    (for {
      costsLoggingProgram <- costL.listen({
                              for {
                                runtime <- Runtime
                                            .create[Task, Task.Par](dbDir, size, StoreType.LMDB)
                                par <- Interpreter[Task].buildNormalizedTerm(contract)
                                res <- Interpreter[Task]
                                        .evaluate(runtime, par, Cost(initialPhlo.toLong))
                                _ <- runtime.close()
                                _ <- Task.eval(dbDir.recursivelyDelete())
                              } yield (res)
                            }.attempt)
      (result, costLog) = costsLoggingProgram
      res               <- errorLog.readAndClearErrorVector
      _                 = Task.now(res should be(Vector.empty))
    } yield ((result, costLog))).unsafeRunSync
  }

  "Total cost of evaluation" should "be equal to the sum of all costs in the log" ignore {
    val initialPhlo = 10000L
    val contract    = """new loop in {
                             contract loop(@n) = {
                               match n {
                                 0 => Nil
                                 _ => loop!(n-1)
                               }
                             } |
                             loop!(10)
                           }""".stripMargin

    val expectedTotalCost = 1766L
    val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
    result shouldBe Right(EvaluateResult(Cost(expectedTotalCost), Vector.empty))
    costLog.map(_.value).toList.sum shouldEqual expectedTotalCost
  }

  "Running out of phlogistons" should "stop the evaluation upon cost depletion in a single execution branch" in {
    val initialPhlo       = 1L
    val contract          = "@1!(1)"
    val expectedCosts     = List(Cost(4, "substitution"))
    val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
    result shouldBe Left(OutOfPhlogistonsError)
    costLog.toList should contain theSameElementsAs (expectedCosts)
  }

  it should "stop the evaluation of all execution branches when one of them runs out of phlo" ignore {
    val initialPhlo       = 1L
    val contract          = "@1!(1) | @2!(2) | @3!(3)"
    val expectedCosts     = List(Cost(4, "substitution"))
    val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
    result shouldBe Left(OutOfPhlogistonsError)
    costLog.toList should contain theSameElementsAs (expectedCosts)
  }

  it should "stop the evaluation of all execution branches when one of them runs out of phlo with a more sophisiticated contract" ignore {
    val initialPhlo = 150L
    val contract    = """new loop in {
                             contract loop(@n) = {
                               match n {
                                 0 => Nil
                                 _ => loop!(n-1)
                               }
                             } |
                             loop!(10)
                           }""".stripMargin

    val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
    result shouldBe Left(OutOfPhlogistonsError)
    val costs = costLog.map(_.value).toList
    // The sum of all costs but last needs to be <= initialPhlo, otherwise
    // the last cost should have not been logged
    costs.init.sum.toLong should be <= (initialPhlo)
    // The sum of ALL costs needs to be > initialPhlo, otherwise an error
    // should not have been reported
    costs.sum.toLong should be > (initialPhlo)
  }
}
