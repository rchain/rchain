package coop.rchain.rholang.interpreter.accounting

import cats.data.Chain
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting.utils._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rspace.Checkpoint
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck._
import org.scalatest.prop.Checkers.check
import org.scalatest.prop.PropertyChecks
import org.scalatest.{AppendedClues, Assertion, FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer
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
    implicit val kvm                       = InMemoryStoreManager[Task]

    val resources = for {
      costLog             <- costLog[Task]()
      cost                <- CostAccounting.emptyCost[Task](implicitly, metricsEff, costLog, ms)
      store               <- kvm.rSpaceStores
      spaces              <- Runtime.setupRSpace[Task](store)
      (rspace, replay, _) = spaces
      runtime <- {
        // naming noOpCostLog because want to override package scope noOpCostLog val
        implicit val c: _cost[Task] = cost
        Runtime.create[Task]((rspace, replay), Seq.empty)
      }
    } yield (runtime, costLog)

    resources
      .flatMap {
        case (runtime, costL) =>
          costL.listen {
            implicit val cost = runtime.cost
            InterpreterUtil.evaluateResult(runtime, contract, Cost(initialPhlo.toLong))
          }
      }
      .runSyncUnsafe(75.seconds)
  }

  def evaluateAndReplay(
      initialPhlo: Cost,
      term: String
  ): (EvaluateResult, EvaluateResult) = {

    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val ms: Metrics.Source        = Metrics.BaseSource
    implicit val kvm                       = InMemoryStoreManager[Task]

    val evaluaResult = for {
      costLog             <- costLog[Task]()
      cost                <- CostAccounting.emptyCost[Task](implicitly, metricsEff, costLog, ms)
      store               <- kvm.rSpaceStores
      spaces              <- Runtime.setupRSpace[Task](store)
      (rspace, replay, _) = spaces
      runtime <- {
        implicit val c: _cost[Task] = cost
        Runtime.create[Task]((rspace, replay), Seq.empty)
      }
      result <- {
        implicit def rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
        implicit val c: _cost[Task]         = cost

        Interpreter[Task].injAttempt(
          runtime.reducer,
          term,
          initialPhlo,
          Map.empty
        )(rand) >>= { playResult =>
          runtime.space.createCheckpoint() >>= {
            case Checkpoint(root, log) =>
              runtime.replaySpace.rigAndReset(root, log) >>
                Interpreter[Task].injAttempt(
                  runtime.replayReducer,
                  term,
                  initialPhlo,
                  Map.empty
                )(rand) >>= { replayResult =>
                runtime.replaySpace.checkReplayData().as((playResult, replayResult))
              }
          }
        }
      }
    } yield result

    evaluaResult.runSyncUnsafe(75.seconds)
  }

  // Uses Godel numbering and a https://en.wikipedia.org/wiki/Mixed_radix
  // to encode certain terms as numbers in the range [0, 0x144000000).
  // Every number gets decoded into a unique term, but some terms can
  // be encoded by more than one number.
  def fromLong(index: Long): String = {
    var remainder = index
    val numPars   = (index % 4) + 1
    remainder /= 4
    val result        = new ListBuffer[String]()
    var nonlinearSend = false;
    var nonlinearRecv = false;
    for (i <- 0 until numPars.toInt) {
      val dir = remainder % 2
      remainder /= 2
      if (dir == 0) {
        // send
        val bang = if (remainder % 2 == 0) "!" else "!!"
        remainder /= 2

        if (bang == "!" || !nonlinearRecv) {
          val ch = remainder % 4
          remainder /= 4
          result += f"@${ch}${bang}(0)"
          nonlinearSend ||= (bang == "!!")
        }
      } else {
        // receive
        val arrow = (remainder % 3) match {
          case 0 => "<-"
          case 1 => "<="
          case 2 => "<<-"
        }
        remainder /= 3

        if (arrow != "<=" || !nonlinearSend) {
          val numJoins = (remainder % 2) + 1
          remainder /= 2

          val joins = new ListBuffer[String]()
          for (j <- 1 to numJoins.toInt) {
            val ch = remainder % 4
            remainder /= 4
            joins += f"_ ${arrow} @${ch}"
          }
          val joinStr = joins.mkString("; ")
          result += f"for (${joinStr}) { 0 }"
          nonlinearRecv ||= (arrow == "<=")
        }
      }
    }
    result.mkString(" | ")
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
    ("""new ret, keccak256Hash(`rho:crypto:keccak256Hash`) in {
       |  keccak256Hash!("TEST".toByteArray(), *ret) |
       |  for (_ <- ret) { Nil }
       |}""".stripMargin, 782L)
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

  it should "be deterministic" in
    forAll(contracts) { (contract: String, _) =>
      checkDeterministicCost {
        val result = evaluateWithCostLog(Integer.MAX_VALUE, contract)
        assert(result._1.errors.isEmpty)
        result
      }
    }

  // TODO: Remove ignore when bug RCHAIN-3917 is fixed.
  it should "be repeatable when generated" ignore {
    val r = scala.util.Random
    // Try contract fromLong(1716417707L) = @2!!(0) | @0!!(0) | for (_ <<- @2) { 0 } | @2!(0)"
    // because the cost is nondeterministic
    val result1 = evaluateAndReplay(Cost(Integer.MAX_VALUE), fromLong(1716417707))
    assert(result1._1.errors.isEmpty)
    assert(result1._2.errors.isEmpty)
    assert(result1._1.cost == result1._2.cost)
    // Try contract fromLong(510661906) = @1!(0) | @1!(0) | for (_ <= @1; _ <= @1) { 0 }
    // because of bug RCHAIN-3917
    val result2 = evaluateAndReplay(Cost(Integer.MAX_VALUE), fromLong(510661906))
    assert(result2._1.errors.isEmpty)
    assert(result2._2.errors.isEmpty)
    assert(result2._1.cost == result2._2.cost)

    for (i <- 1 to 10000) {
      val long     = ((r.nextLong % 0X144000000L) + 0X144000000L) % 0X144000000L
      val contract = fromLong(long)
      if (contract != "") {
        val result = evaluateAndReplay(Cost(Integer.MAX_VALUE), contract)
        assert(result._1.errors.isEmpty)
        assert(result._2.errors.isEmpty)
        assert(result._1.cost == result._2.cost)
      }
    }
  }

  def checkDeterministicCost(block: => (EvaluateResult, Chain[Cost])): Unit = {
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
    totalCost.value should be >= initialPhlo
  }

  private def elementCounts[A](list: Iterable[A]): Set[(A, Int)] =
    list.groupBy(identity).mapValues(_.size).toSet

  it should "stop the evaluation of all execution branches when one of them runs out of phlo with a more sophisiticated contract" in {
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
