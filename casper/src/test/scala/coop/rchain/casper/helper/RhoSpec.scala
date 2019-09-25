package coop.rchain.casper.helper

import cats.implicits._
import coop.rchain.casper.genesis.contracts.TestUtil
import coop.rchain.casper.genesis.contracts.TestUtil.eval
import coop.rchain.casper.protocol.{DeployData, DeployDataProto}
import coop.rchain.casper.util.GenesisBuilder.GenesisParameters
import coop.rchain.casper.util.rholang.Resources.copyStorage
import coop.rchain.casper.util.{GenesisBuilder, ProtoUtil}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.Resources.mkRuntimeAt
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.{NormalizerEnv, PrettyPrinter, Runtime}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration

class RhoSpec(
    testObject: CompiledRholangSource,
    executionTimeout: FiniteDuration,
    genesisParameters: GenesisParameters = GenesisBuilder.buildGenesisParameters()
) extends FlatSpec
    with AppendedClues
    with Matchers {

  implicit val logger: Log[Task]         = Log.log[Task]
  implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

  private val printer = PrettyPrinter()

  def mkTest(test: (String, Map[Long, List[RhoTestAssertion]])): Unit =
    test match {
      case (testName, testAttempts) =>
        assert(testAttempts.nonEmpty, "It doesn't make sense to have less than one attempt")

        val (attempt, assertions) =
          testAttempts.find(_._2.exists(!_.isSuccess)).getOrElse(testAttempts.head)

        def clueMsg(clue: String) = s"$clue (test attempt: $attempt)"

        it should testName in {
          assertions.foreach {
            case RhoAssertEquals(_, expected, actual, clue) =>
              printer.buildString(actual) should be(printer.buildString(expected)) withClue clueMsg(
                clue
              )
              actual should be(expected) withClue clueMsg(clue)
            case RhoAssertNotEquals(_, unexpected, actual, clue) =>
              printer.buildString(actual) should not be printer
                .buildString(unexpected) withClue clueMsg(clue)
              actual should not be unexpected withClue clueMsg(clue)
            case RhoAssertTrue(_, v, clue) =>
              v should be(true) withClue clueMsg(clue)
          }
        }
    }

  private def testFrameworkContracts(
      testResultCollector: TestResultCollector[Task]
  ): Seq[SystemProcess.Definition[Task]] =
    Seq((5, "assertAck", 101), (1, "testSuiteCompleted", 102))
      .map {
        case (arity, name, n) =>
          SystemProcess.Definition[Task](
            s"rho:test:$name",
            Runtime.byteName(n.toByte),
            arity,
            n.toLong,
            ctx => testResultCollector.handleMessage(ctx)(_, _)
          )
      } ++ Seq(
      SystemProcess.Definition[Task](
        "rho:io:stdlog",
        Runtime.byteName(103),
        2,
        103L,
        ctx => RhoLoggerContract.handleMessage(ctx)(_, _)
      ),
      SystemProcess.Definition[Task](
        "rho:test:deploy:set",
        Runtime.byteName(104),
        3,
        104L,
        ctx => DeployDataContract.set(ctx)(_, _)
      ),
      SystemProcess.Definition[Task](
        "rho:test:deployerId:get",
        Runtime.byteName(105),
        3,
        105L,
        ctx => DeployerIdContract.get(ctx)(_, _)
      ),
      SystemProcess.Definition[Task](
        "rho:test:crypto:secp256k1Sign",
        Runtime.byteName(106),
        3,
        106L,
        ctx => Secp256k1SignContract.get(ctx)(_, _)
      )
    )

  private def getResults: Task[TestResult] =
    TestResultCollector[Task] >>= { testResultCollector =>
      val runtimeResource = for {
        storageDirs <- copyStorage[Task](
                        GenesisBuilder.buildGenesis(genesisParameters).storageDirectory
                      )
        runtime <- mkRuntimeAt[Task](storageDirs.rspaceDir)(
                    storageSize = 10 * 1024 * 1024,
                    additionalSystemProcesses = testFrameworkContracts(testResultCollector)
                  )
      } yield runtime

      runtimeResource.use { runtime =>
        for {
          _    <- logger.info("Starting tests from " + testObject.path)
          _    <- setupRuntime(runtime)
          rand = Blake2b512Random(128)
          _ <- TestUtil
                .eval(testObject.code, runtime, testObject.normalizerEnv)(
                  implicitly,
                  rand.splitShort(1)
                )
                .timeout(executionTimeout)

          result <- testResultCollector.getResult
        } yield result
      }
    }

  private def setupRuntime(runtime: Runtime[Task]): Task[Runtime[Task]] =
    for {
      _ <- evalDeploy(rhoSpecDeploy, runtime)
      // reset the deployParams.userId before executing the test
      // otherwise it'd execute as the deployer of last deployed contract
      _ <- runtime.deployParametersRef.update(_.copy(userId = Par()))
    } yield runtime

  private def evalDeploy(
      deploy: DeployData,
      runtime: Runtime[Task]
  ): Task[Unit] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      ProtoUtil.stripDeployData(deploy).toProto.toByteArray
    )
    eval(deploy.term, runtime, NormalizerEnv(deploy.toProto))
  }

  private val rhoSpecDeploy: DeployData =
    DeployData.from(
      DeployDataProto(
        deployer = ProtoUtil.stringToByteString(
          "0401f5d998c9be9b1a753771920c6e968def63fe95b20c71a163a7f7311b6131ac65a49f796b5947fa9d94b0542895e7b7ebe8b91eefcbc5c7604aaf281922ccac"
        ),
        timestamp = 1559158671800L,
        term = CompiledRholangSource("RhoSpecContract.rho", NormalizerEnv.Empty).code
      )
    )

  val result: TestResult = getResults.runSyncUnsafe()

  it should "finish execution within timeout" in {
    if (!result.hasFinished) fail(s"Timeout of $executionTimeout expired")
  }

  result.assertions.foreach(mkTest)
}
