package coop.rchain.casper.helper

import coop.rchain.casper.genesis.contracts.TestUtil
import coop.rchain.casper.protocol.DeployData
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration.Duration

object RhoSpec {
  implicit val logger: Log[Task] = Log.log[Task]

  private def mkRuntime(testResultCollector: TestResultCollector[Task]) = {
    val testResultCollectorService =
      Seq((5, "assertAck", 25), (1, "testSuiteCompleted", 26))
        .map {
          case (arity, name, n) =>
            SystemProcess.Definition[Task](
              s"rho:test:$name",
              Runtime.byteName(n.toByte),
              arity,
              n.toLong,
              ctx => testResultCollector.handleMessage(ctx)(_,_)
            )
        } ++ Seq(
        SystemProcess.Definition[Task](
          "rho:io:stdlog",
          Runtime.byteName(27),
          2,
          27L,
          ctx => RhoLogger.handleMessage(ctx)(_,_)
        )
      )
    TestUtil.runtime(testResultCollectorService)
  }

  def getResults(testObject: CompiledRholangSource, otherLibs: Seq[DeployData]): Task[TestResult] =
    for {
      testResultCollector <- TestResultCollector[Task]

      _ <- Task.delay {
            TestUtil.runTestsWithDeploys(testObject, otherLibs, mkRuntime(testResultCollector))
          }

      result <- testResultCollector.getResult
    } yield result
}

class RhoSpec(
    testObject: CompiledRholangSource,
    standardDeploys: Seq[DeployData],
    executionTimeout: Duration
) extends FlatSpec
    with AppendedClues
    with Matchers {
  def mkTest(test: (String, Map[Long, List[RhoTestAssertion]])): Unit =
    test match {
      case (testName, testAttempts) =>
        assert(testAttempts.size > 0, "It doesn't make sense to have less than one attempt")

        val (attempt, assertions) =
          testAttempts
            .find { case (attempt, assertions) => hasFailures(assertions) }
            .getOrElse(testAttempts.head)

        def clueMsg(clue: String) = s"$clue (test attempt: $attempt)"

        it should testName in {
          assertions.foreach {
            case RhoAssertEquals(_, expected, actual, clue) =>
              actual should be(expected) withClue clueMsg(clue)
            case RhoAssertNotEquals(_, unexpected, actual, clue) =>
              actual should not be (unexpected) withClue clueMsg(clue)
            case RhoAssertTrue(_, v, clue) => v should be(true) withClue clueMsg(clue)
          }
        }
    }

  def hasFailures(assertions: List[RhoTestAssertion]) = assertions.find(_.isSuccess).isDefined

  private val result = RhoSpec
    .getResults(testObject, standardDeploys)
    .runSyncUnsafe(executionTimeout)

  it should "finish execution within timeout" in {
    if (!result.hasFinished) fail(s"Timeout of $executionTimeout expired")
  }

  result.assertions
    .foreach(mkTest)
}
