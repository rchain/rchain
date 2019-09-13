package coop.rchain.rholang.interpreter.accounting

import cats.data.Chain
import cats.effect._
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting.utils._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck._
import org.scalatest.prop.Checkers.check
import org.scalatest.prop.PropertyChecks
import org.scalatest.{AppendedClues, Assertion, FlatSpec, Matchers}

import scala.concurrent.duration._

class CostAccountingSpec extends FlatSpec with Matchers with PropertyChecks with AppendedClues {

  private[this] def evaluateWithCostLog(
      initialPhlo: Long,
      contract: String
  ): (EvaluateResult, Chain[Cost]) = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val ms: Metrics.Source        = Metrics.BaseSource

    val resources = for {
      dir     <- Resources.mkTempDir[Task]("cost-accounting-spec-")
      costLog <- Resource.liftF(costLog[Task]())
      cost    <- Resource.liftF(CostAccounting.emptyCost[Task](implicitly, metricsEff, costLog, ms))
      sar     <- Resource.liftF(Runtime.setupRSpace[Task](dir, 10 * 1024 * 1024))
      runtime <- {
        implicit val c = cost
        Resource.make(Runtime.create[Task, Task.Par](sar, Nil))(_.close())
      }
    } yield (runtime, costLog)

    resources
      .use {
        case (runtime, costL) =>
          costL.listen {
            implicit val cost = runtime.cost
            InterpreterUtil.evaluateResult(runtime, contract, Cost(initialPhlo.toLong))
          }
      }
      .runSyncUnsafe(75.seconds)
  }

  val contracts = Table(
    ("contract", "expectedTotalCost"),
    ("""@0!(2)""", 97L),
    ("""@0!(2) | @1!(1)""", 197L),
    ("""for(x <- @0){ Nil }""", 128L),
    ("""for(x <- @0){ Nil } | @0!(2)""", 329L),
    ("@0!!(0) | for (_ <- @0) { 0 }", 342L),
    ("@0!!(0) | for (x <- @0) { 0 }", 342L),
    ("@0!!(0) | for (@0 <- @0) { 0 }", 336L),
    ("@0!!(0) | @0!!(0) | for (_ <- @0) { 0 }", 443L),
    ("@0!!(0) | @1!!(1) | for (_ <- @0;_ <- @1) { 0 }", 594L),
    ("@0!(0) | for (_ <- @0) { 0 }", 333L),
    ("@0!(0) | for (x <- @0) { 0 }", 333L),
    ("@0!(0) | for (@0 <- @0) { 0 }", 327L),
    ("@0!(0) | for (_ <= @0) { 0 }", 354L),
    ("@0!(0) | for (x <= @0) { 0 }", 356L),
    ("@0!(0) | for (@0 <= @0) { 0 }", 341L),
    ("@0!(0) | @0!(0) | for (_ <= @0) { 0 }", 574L),
    ("@0!(0) | for (@0 <- @0) { 0 } | @0!(0) | for (_ <- @0) { 0 }", 663L),
    ("@0!(0) | for (@0 <- @0) { 0 } | @0!(0) | for (@1 <- @0) { 0 }", 551L),
    ("@0!(0) | for (_ <<- @0) { 0 }", 406L),
    ("@0!!(0) | for (_ <<- @0) { 0 }", 343L),
    ("@0!!(0) | @0!!(0) | for (_ <<- @0) { 0 }", 444L),
    ("""new loop in {
         contract loop(@n) = {
           match n {
             0 => Nil
             _ => loop!(n-1)
           }
         } |
         loop!(10)
       }""".stripMargin, 3892L),
    ("""42 | @0!(2) | for (x <- @0) { Nil }""", 336L),
    ("""@1!(1) |
        for(x <- @1) { Nil } |
        new x in { x!(10) | for(X <- x) { @2!(Set(X!(7)).add(*X).contains(10)) }} |
        match 42 {
          38 => Nil
          42 => @3!(42)
        }
     """.stripMargin, 1264L),
    // test that we charge for system processes
    ("""new ret in {
       |  @"keccak256Hash"!("TEST".toByteArray(), *ret) |
       |  for (_ <- ret) { Nil }
       |}""".stripMargin, 734L)
    // TODO add a test making sure registry usage has deterministic cost too
  )

  "Total cost of evaluation" should "be equal to the sum of all costs in the log" in {
    forAll(contracts) { (contract: String, expectedTotalCost: Long) =>
      {
        val initialPhlo       = 10000L
        val (result, costLog) = evaluateWithCostLog(initialPhlo, contract)
        result shouldBe EvaluateResult(Cost(expectedTotalCost), Vector.empty)
        costLog.map(_.value).toList.sum shouldEqual expectedTotalCost
      }
    }
  }

  it should "be repeatable" in
    forAll(contracts) { (contract: String, _) =>
      checkRepeatableCost {
        val result = evaluateWithCostLog(Integer.MAX_VALUE, contract)
        assert(result._1.errors.isEmpty)
        result
      }
    }

  def checkRepeatableCost(block: => (EvaluateResult, Chain[Cost])): Unit = {
    val repetitions = 20
    val first       = block
    // execute in parallel to trigger different interleaves
    val subsequents = (1 to repetitions).par.map(_ => block).toList
    // check assertions sequentially to avoid "suppressed exceptions" output on assertion failure
    subsequents.foreach { subsequent =>
      val expected = first._1.cost.value
      val actual   = subsequent._1.cost.value
      if (expected != actual) {
        assert(subsequent._2.map(_ + "\n") == first._2.map(_ + "\n"))
          .withClue(s"Cost was not repeatable, expected $expected, got $actual.\n")
      }
    }
  }

  "Running out of phlogistons" should "stop the evaluation upon cost depletion in a single execution branch" in {
    val parsingCost = 6L
    checkPhloLimitExceeded(
      "@1!(1)",
      parsingCost,
      List(Cost(parsingCost, "parsing"))
    )
  }

  it should "not attempt reduction when there wasn't enough phlo for parsing" in {
    val parsingCost = 6L
    checkPhloLimitExceeded("@1!(1)", parsingCost - 1, List())
  }

  it should "stop the evaluation of all execution branches when one of them runs out of phlo" in {
    val parsingCost   = 24L
    val firstStepCost = 11
    checkPhloLimitExceeded(
      "@1!(1) | @2!(2) | @3!(3)",
      parsingCost + firstStepCost,
      List(Cost(parsingCost, "parsing"), Cost(firstStepCost, "send eval"))
    )
  }

  private def checkPhloLimitExceeded(
      contract: String,
      initialPhlo: Long,
      expectedCosts: Seq[Cost]
  ): Assertion = {
    val (EvaluateResult(totalCost, errors), costLog) = evaluateWithCostLog(initialPhlo, contract)
    withClue("We must not expect more costs than initialPhlo allows (duh!):\n") {
      expectedCosts.map(_.value).sum should be <= initialPhlo
    }
    errors shouldBe List(OutOfPhlogistonsError)
    costLog.toList should contain allElementsOf expectedCosts
    withClue("Exactly one cost should be logged past the expected ones, yet:\n") {
      elementCounts(costLog.toList) diff elementCounts(expectedCosts) should have size 1
    }
    totalCost.value should be > initialPhlo
  }

  private def elementCounts[A](list: Iterable[A]): Set[(A, Int)] =
    list.groupBy(identity).mapValues(_.size).toSet

  // FIXME make this pass consistently - https://rchain.atlassian.net/browse/RCHAIN-3790
  it should "stop the evaluation of all execution branches when one of them runs out of phlo with a more sophisiticated contract" ignore {
    forAll(contracts) { (contract: String, expectedTotalCost: Long) =>
      check(forAllNoShrink(Gen.choose(1L, expectedTotalCost - 1)) { initialPhlo =>
        val (EvaluateResult(_, errors), costLog) =
          evaluateWithCostLog(initialPhlo, contract)
        errors shouldBe List(OutOfPhlogistonsError)
        val costs = costLog.map(_.value).toList
        // The sum of all costs but last needs to be <= initialPhlo, otherwise
        // the last cost should have not been logged
        costs.init.sum should be <= initialPhlo withClue s", cost log was: $costLog"

        // The sum of ALL costs needs to be > initialPhlo, otherwise an error
        // should not have been reported
        costs.sum > initialPhlo withClue s", cost log was: $costLog"
      })
    }
  }

}
