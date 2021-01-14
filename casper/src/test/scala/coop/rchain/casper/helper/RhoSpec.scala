package coop.rchain.casper.helper

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.genesis.contracts.TestUtil
import coop.rchain.casper.genesis.contracts.TestUtil.eval
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.GenesisBuilder.GenesisParameters
import coop.rchain.casper.util.rholang.Resources.{copyStorage, mkTestRNodeStoreManager}
import coop.rchain.casper.util.rholang.Tools
import coop.rchain.casper.util.{GenesisBuilder, ProtoUtil}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.Resources.mkRuntimeAt
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.{PrettyPrinter, Runtime}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration.{Duration, FiniteDuration}

class RhoSpec(
    testObject: CompiledRholangSource[_],
    extraNonGenesisDeploys: Seq[Signed[DeployData]],
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

  private def testFrameworkContracts[F[_]: Log: Concurrent: Span](
      testResultCollector: TestResultCollector[F]
  ): Seq[SystemProcess.Definition[F]] = {
    val testResultCollectorService =
      Seq((5, "assertAck", 101), (1, "testSuiteCompleted", 102))
        .map {
          case (arity, name, n) =>
            SystemProcess.Definition[F](
              s"rho:test:$name",
              Runtime.byteName(n.toByte),
              arity,
              n.toLong,
              ctx => testResultCollector.handleMessage(ctx)(_)
            )
        } ++ Seq(
        SystemProcess.Definition[F](
          "rho:io:stdlog",
          Runtime.byteName(103),
          2,
          103L,
          ctx => RhoLoggerContract.handleMessage(ctx)(_)
        ),
        SystemProcess.Definition[F](
          "rho:test:deployerId:make",
          Runtime.byteName(104),
          3,
          104L,
          ctx => DeployerIdContract.get(ctx)(_)
        ),
        SystemProcess.Definition[F](
          "rho:test:crypto:secp256k1Sign",
          Runtime.byteName(105),
          3,
          105L,
          ctx => Secp256k1SignContract.get(ctx)(_)
        ),
        SystemProcess.Definition[F](
          "sys:test:authToken:make",
          Runtime.byteName(106),
          1,
          106L,
          ctx => SysAuthTokenContract.get(ctx)(_)
        ),
        SystemProcess.Definition[F](
          "rho:test:block:data:set",
          Runtime.byteName(107),
          3,
          107L,
          ctx => BlockDataContract.set(ctx)(_)
        ),
        SystemProcess.Definition[F](
          "rho:test:casper:invalidBlocks:set",
          Runtime.byteName(108),
          2,
          108L,
          ctx => CasperInvalidBlocksContract.set(ctx)(_)
        )
      )
    testResultCollectorService
  }

  private def getResults(
      testObject: CompiledRholangSource[_],
      otherLibs: Seq[Signed[DeployData]],
      timeout: FiniteDuration
  ): Task[TestResult] =
    TestResultCollector[Task].flatMap { testResultCollector =>
      val genesis = GenesisBuilder.buildGenesis(genesisParameters)

      val runtimeResource = copyStorage[Task](genesis.storageDirectory)
        .map(_.storageDir)
        .evalMap(mkTestRNodeStoreManager[Task])
        .evalMap(
          mkRuntimeAt[Task](
            _,
            additionalSystemProcesses = testFrameworkContracts(testResultCollector)
          )
        )
        .map(_._1)

      runtimeResource.use { runtime =>
        for {
          _ <- logger.info("Starting tests from " + testObject.path)
          _ <- setupRuntime[Task](
                runtime,
                otherLibs
              )
          rand = Blake2b512Random(128)
          _ <- TestUtil
                .eval(testObject, runtime)(
                  implicitly,
                  rand.splitShort(1)
                )
                .timeout(timeout)

          result <- testResultCollector.getResult
        } yield result
      }
    }

  private def setupRuntime[F[_]: Sync](
      runtime: Runtime[F],
      otherLibs: Seq[Signed[DeployData]]
  ): F[Runtime[F]] =
    for {
      _ <- evalDeploy(rhoSpecDeploy, runtime)
      _ <- otherLibs.toList.traverse(evalDeploy(_, runtime))
    } yield runtime

  private def evalDeploy[F[_]: Sync](
      deploy: Signed[DeployData],
      runtime: Runtime[F]
  ): F[Unit] = {
    import coop.rchain.models.rholang.implicits._
    val rand = Tools.unforgeableNameRng(deploy.pk, deploy.data.timestamp)
    eval(deploy.data.term, runtime, NormalizerEnv(deploy).toEnv)(Sync[F], rand)
  }

  private val rhoSpecDeploy: Signed[DeployData] = {
    val sk = PrivateKey(
      ProtoUtil.stringToByteString(
        "abaa20c1d578612b568a7c3d9b16e81c68d73b931af92cf79727e02011c558c6"
      )
    )

    Signed(
      DeployData(
        timestamp = 1559158671800L,
        term = CompiledRholangSource("RhoSpecContract.rho", NormalizerEnv.Empty).code,
        phloLimit = Long.MaxValue,
        phloPrice = 0,
        validAfterBlockNumber = 0
      ),
      Secp256k1,
      sk
    )
  }

  val result =
    getResults(testObject, extraNonGenesisDeploys, executionTimeout).runSyncUnsafe(Duration.Inf)

  it should "finish execution within timeout" in {
    if (!result.hasFinished) fail(s"Timeout of $executionTimeout expired")
  }

  result.assertions
    .foreach(mkTest)
}
