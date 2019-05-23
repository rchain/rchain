package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.casper.MultiParentCasperTestUtil.createBonds
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{Faucet, PreWallet, ProofOfStake, TestUtil, Validator}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.casper.genesis.contracts.TestUtil
import coop.rchain.casper.protocol.DeployData
import coop.rchain.metrics.Metrics
import coop.rchain.rholang.Resources._
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration.{Duration, FiniteDuration}
import coop.rchain.rholang.interpreter.PrettyPrinter

object RhoSpec {

  implicit val logger: Log[Task]         = Log.log[Task]
  implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]

  private def testFrameworkContracts[F[_]: Log: Concurrent](
      testResultCollector: TestResultCollector[F]
  ): Seq[SystemProcess.Definition[F]] = {
    val testResultCollectorService =
      Seq((5, "assertAck", 25), (1, "testSuiteCompleted", 26))
        .map {
          case (arity, name, n) =>
            SystemProcess.Definition[F](
              s"rho:test:$name",
              Runtime.byteName(n.toByte),
              arity,
              n.toLong,
              ctx => testResultCollector.handleMessage(ctx)(_, _)
            )
        } ++ Seq(
        SystemProcess.Definition[F](
          "rho:io:stdlog",
          Runtime.byteName(27),
          2,
          27L,
          ctx => RhoLoggerContract.handleMessage(ctx)(_, _)
        ),
        SystemProcess.Definition[F](
          "rho:test:deploy:set",
          Runtime.byteName(28),
          3,
          28L,
          ctx => DeployDataContract.set(ctx)(_, _)
        )
      )
    testResultCollectorService
  }

  def getResults(
      testObject: CompiledRholangSource,
      otherLibs: Seq[DeployData],
      timeout: FiniteDuration
  ): Task[TestResult] =
    TestResultCollector[Task].flatMap { testResultCollector =>
      mkRuntime[Task, Task.Par](
        s"rhoSpec-${testObject.path}",
        10 * 1024 * 1024,
        testFrameworkContracts(testResultCollector)
      ).use { runtime =>
        for {

          _ <- logger.info("Starting tests from " + testObject.path)
          _ <- Runtime.injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
          _ <- TestUtil.setupRuntime[Task, Task.Par](
                runtime,
                TestUtil.defaultGenesisSetup,
                otherLibs
              )
          rand = Blake2b512Random(128)
          _ <- TestUtil
                .eval(testObject.code, runtime)(implicitly, rand.splitShort(1))
                .timeout(timeout)

          result <- testResultCollector.getResult
        } yield result
      }
    }
}

class RhoSpec(
    testObject: CompiledRholangSource,
    extraNonGenesisDeploys: Seq[DeployData],
    executionTimeout: FiniteDuration
) extends FlatSpec
    with AppendedClues
    with Matchers {

  private val printer = PrettyPrinter()

  def mkTest(test: (String, Map[Long, List[RhoTestAssertion]])): Unit =
    test match {
      case (testName, testAttempts) =>
        assert(testAttempts.size > 0, "It doesn't make sense to have less than one attempt")

        val (attempt, assertions) =
          testAttempts
            .find { case (_, assertions) => hasFailures(assertions) }
            .getOrElse(testAttempts.head)

        def clueMsg(clue: String) = s"$clue (test attempt: $attempt)"

        it should testName in {
          assertions.foreach {
            case RhoAssertEquals(_, expected, actual, clue) =>
              printer.buildString(actual) should be(printer.buildString(expected)) withClue clueMsg(
                clue
              )
              actual should be(expected) withClue clueMsg(clue)
            case RhoAssertNotEquals(_, unexpected, actual, clue) =>
              printer.buildString(actual) should not be (printer
                .buildString(unexpected)) withClue clueMsg(clue)
              actual should not be (unexpected) withClue clueMsg(clue)
            case RhoAssertTrue(_, v, clue) =>
              v should be(true) withClue clueMsg(clue)
          }
        }
    }

  def hasFailures(assertions: List[RhoTestAssertion]) = assertions.find(_.isSuccess).isDefined

  private val result = RhoSpec
    .getResults(testObject, extraNonGenesisDeploys, executionTimeout)
    .runSyncUnsafe(Duration.Inf)

  it should "finish execution within timeout" in {
    if (!result.hasFinished) fail(s"Timeout of $executionTimeout expired")
  }

  result.assertions
    .foreach(mkTest)
}
