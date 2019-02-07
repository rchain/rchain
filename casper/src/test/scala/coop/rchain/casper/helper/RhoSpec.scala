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
      Seq((4, "assertAck", 24), (1, "testSuiteCompleted", 25))
        .map {
          case (arity, name, n) =>
            SystemProcess.Definition[Task](
              s"rho:test:$name",
              Runtime.byteName(n.toByte),
              arity,
              n.toLong,
              testResultCollector.handleMessage
            )
        } ++ Seq(
        SystemProcess.Definition[Task](
          "rho:io:stdlog",
          Runtime.byteName(26),
          2,
          26L,
          RhoLogger.handleMessage[Task]
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
  def mkTest(test: (String, List[RhoTestAssertion])): Unit =
    test match {
      case (testName, assertions) =>
        it should testName in {
          assertions.foreach {
            case RhoAssertEquals(_, expected, actual, clue) =>
              actual should be(expected) withClue clue
            case RhoAssertTrue(_, v, clue) => v should be(true) withClue clue
          }
        }
    }

  private val result = RhoSpec
    .getResults(testObject, standardDeploys)
    .runSyncUnsafe(executionTimeout)

  it should "finish execution within timeout" in {
    result.hasFinished should be(true) withClue s"timeout of $executionTimeout expired"
  }

  result.assertions.foreach(mkTest)
}
