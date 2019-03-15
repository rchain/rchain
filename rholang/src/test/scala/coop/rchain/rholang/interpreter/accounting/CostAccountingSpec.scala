package coop.rchain.rholang.interpreter.accounting

import java.nio.file.Files

import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.accounting.utils._
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.shared.StoreType
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAllNoShrink
import org.scalactic.TripleEqualsSupport
import org.scalatest._
import org.scalatest.prop.Checkers.check
import org.scalatest.prop.PropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration._

class CostAccountingSpec extends FlatSpec with Matchers with PropertyChecks {

  private[this] def evaluateWithCostLog(
      initialPhlo: Long,
      contract: String
  ) = {
    val dbDir                              = Files.createTempDirectory("cost-accounting-spec-")
    val size                               = 1024L * 1024 * 1024
    implicit val errorLog                  = new ErrorLog[Task]()
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    (for {
      costAlg <- CostAccounting.empty[Task]
      costL   <- costLog[Task]
      cost    = loggingCost(costAlg, costL)
      costsLoggingProgram <- {
        costL.listen({
          implicit val c = cost
          for {
            runtime <- Runtime
                        .create[Task, Task.Par](dbDir, size, StoreType.LMDB)
            res <- Interpreter[Task]
                    .evaluate(runtime, contract, Cost(initialPhlo.toLong))
            _ <- runtime.close()
            _ <- Task.eval(dbDir.recursivelyDelete())
          } yield (res)
        })
      }
      (result, costLog) = costsLoggingProgram
      res               <- errorLog.readAndClearErrorVector
      _                 <- Task.now(res should be(Vector.empty))
    } yield ((result, costLog))).runSyncUnsafe(25.seconds)
  }

  val contracts = Table(
    ("contract", "expectedTotalCost"),
    ("""@0!(2)""", 33L),
    ("""@0!(2) | @1!(1)""", 69L),
    ("""for(x <- @0){ Nil }""", 64L),
    ("""for(x <- @0){ Nil } | @0!(2)""", 76L),
    ("""new loop in {
         contract loop(@n) = {
           match n {
             0 => Nil
             _ => loop!(n-1)
           }
         } |
         loop!(10)
       }""".stripMargin, 1936L),
    ("""42 | @0!(2) | for (x <- @0) { Nil }""", 83L),
    ("""@1!(1) |
        for(x <- @1) { Nil } |
        new x in { x!(10) | for(X <- x) { @2!(Set(X!(7)).add(*X).contains(10)) }} |
        match 42 {
          38 => Nil
          42 => @3!(42)
        }
     """.stripMargin, 634L)
  )

  "Total cost of evaluation" should "be equal to the sum of all costs in the log" in forAll(
    contracts
  ) { (contract: String, expectedTotalCost: Long) =>
    val initialPhlo       = 10000L
    val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
    result shouldBe EvaluateResult(Cost(expectedTotalCost), Vector.empty)
    costLog.map(_.value).toList.sum shouldEqual expectedTotalCost
  }

  "Running out of phlogistons" should "stop the evaluation upon cost depletion in a single execution branch" in {
    val contract                                     = "@1!(1)"
    val parsingCost                                  = accounting.parsingCost(contract).value
    val initialPhlo                                  = 1L + parsingCost
    val expectedCosts                                = List(Cost(6, "parsing"), Cost(4, "substitution"))
    val (EvaluateResult(totalCost, errors), costLog) = evaluateWithCostLog(initialPhlo, contract)
    totalCost.value shouldBe expectedCosts.map(_.value).sum
    errors shouldBe (List(OutOfPhlogistonsError))
    costLog.toList should contain theSameElementsAs (expectedCosts)
  }

  it should "not attempt reduction when there wasn't enought phlo for parsing" in {
    val contract                                     = "@1!(1)"
    val parsingCost                                  = accounting.parsingCost(contract).value
    val initialPhlo                                  = parsingCost - 1
    val expectedCosts                                = List(Cost(6, "parsing"))
    val (EvaluateResult(totalCost, errors), costLog) = evaluateWithCostLog(initialPhlo, contract)
    totalCost.value shouldBe expectedCosts.map(_.value).sum
    errors shouldBe (List(OutOfPhlogistonsError))
    costLog.toList should contain theSameElementsAs (expectedCosts)
  }

  it should "stop the evaluation of all execution branches when one of them runs out of phlo" in {
    val contract    = "@1!(1) | @2!(2) | @3!(3)"
    val initialPhlo = 5L + parsingCost(contract).value
    val expectedCosts =
      List(parsingCost(contract), Cost(4, "substitution"), Cost(4, "substitution"))
    val (EvaluateResult(_, errors), costLog) = evaluateWithCostLog(initialPhlo, contract)
    errors shouldBe (List(OutOfPhlogistonsError))
    costLog.toList should contain theSameElementsAs (expectedCosts)
  }

  it should "stop the evaluation of all execution branches when one of them runs out of phlo with a more sophisiticated contract" in forAll(
    contracts
  ) { (contract: String, expectedTotalCost: Long) =>
    check(forAllNoShrink(Gen.choose(1L, expectedTotalCost - 1)) { initialPhlo =>
      val (EvaluateResult(_, errors), costLog) =
        evaluateWithCostLog(initialPhlo, contract)
      errors shouldBe (List(OutOfPhlogistonsError))
      val costs = costLog.map(_.value).toList
      // The sum of all costs but last needs to be <= initialPhlo, otherwise
      // the last cost should have not been logged
      costs.init.sum.toLong should be <= (initialPhlo)

      // The sum of ALL costs needs to be > initialPhlo, otherwise an error
      // should not have been reported
      costs.sum.toLong > (initialPhlo)
    })
  }

}
