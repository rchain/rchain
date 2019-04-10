package coop.rchain.rholang.interpreter.accounting

import java.nio.file.Files

import cats.effect._
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

class CostAccountingSpec extends FlatSpec with Matchers with PropertyChecks with AppendedClues {

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
      costL <- costLog[Task]
      cost  <- CostAccounting.emptyCost[Task](Concurrent[Task], costL)
      costsLoggingProgram <- {
        costL.listen({
          implicit val c = cost
          for {
            runtime <- Runtime
                        .create[Task, Task.Par](dbDir, size, StoreType.LMDB)
            res <- Interpreter[Task]
                    .evaluate(runtime, contract, Cost(initialPhlo.toLong))
            _ <- Task.delay(runtime.close())
            _ <- Task.now(dbDir.recursivelyDelete())
          } yield (res)
        })
      }
      (result, costLog) = costsLoggingProgram
      res               <- errorLog.readAndClearErrorVector
      _                 <- Task.now(res should be(Vector.empty))
    } yield ((result, costLog))).runSyncUnsafe(900000.seconds)
  }

  val contracts = Table(
    ("contract", "expectedTotalCost"),
    ("""new foo in {
         contract foo(@n) = {
           match n {
             0 => Nil
             _ => foo!(0)
           }
         } |
         foo!(1)
       }""".stripMargin, 821L)
  )

  "Total cost of evaluation" should "be deterministic when we expect it to be deterministic" in {
    (0 until 100).foreach { i =>
        println(i)
        val initialPhlo = 10000L
        val contract =
          """
         contract @"foo"(@n) = {
           match n {
             0 => Nil
             1 => @"foo"!(0)
           }
         } |
         @"foo"!(1)""".stripMargin
        val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
        costLog.map { cost => println(s"${cost.operation}, ${cost.value}")}
        result shouldBe EvaluateResult(Cost(521L), Vector.empty)
    }
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
      costs.init.sum.toLong should be <= (initialPhlo) withClue (s", cost log was: $costLog")

      // The sum of ALL costs needs to be > initialPhlo, otherwise an error
      // should not have been reported
      costs.sum.toLong > (initialPhlo) withClue (s", cost log was: $costLog")
    })
  }

}
