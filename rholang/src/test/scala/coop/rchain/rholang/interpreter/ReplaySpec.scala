package coop.rchain.rholang.interpreter

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.SoftCheckpoint
import coop.rchain.shared.Log
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class ReplaySpec extends AnyFlatSpec with Matchers {

  // TODO: these tests are temporary and specific to bugs found in replay.
  // Testing execution for many iteration doesn't make sense.
  // In tests, nondeterministic execution of Par in tuple space should be replaced with deterministic version
  // so that we can test tuple space access in all possible states (and not guess and wait).

  // TODO: fuzzer test now creates complete on-disk tuple space for each execution, this can be replaced
  //  with in-memory version used here.
  // https://github.com/rchain/rchain/blob/1f9554f68a/rholang/src/test/scala/coop/rchain/rholang/interpreter/accounting/CostAccountingSpec.scala#L231

  // Temporarily disabled tests with joins on the same channels
  // https://rchain.atlassian.net/browse/RCHAIN-4032

  "multi joins (2/2)" should "execute successfully in replay" ignore {
    val term =
      """
        |new x in {
        |  x!() | for(<- x & <- x) { 0 } | x!()
        |}
        |""".stripMargin
    testRholangTerm(term, 500, 30.seconds)
  }

  "multi joins (4/2)" should "execute successfully in replay" ignore {
    val term =
      """
        |new x in {
        |  x!() | x!() | for(<- x & <- x) { 0 } | x!() | x!()
        |}
        |""".stripMargin
    testRholangTerm(term, 500, 30.seconds)
  }

  def testRholangTerm(term: String, iterations: Int, timeout: Duration) =
    withRSpaceAndRuntime {
      case (runtime, replayRuntime) =>
        for (i <- 1 to iterations) {
          val (playRes, replayRes) =
            evaluateWithRuntime(runtime, replayRuntime)(term, Cost(Integer.MAX_VALUE))
              .onError {
                case _: Throwable =>
                  println(s"Test retry count: $i").pure[IO]
              }
              .timeout(1.seconds)
              .unsafeRunSync()

          assert(playRes.errors.isEmpty)
          assert(replayRes.errors.isEmpty)
        }
        ().pure[IO]
    }.timeout(timeout).unsafeRunSync()

  def evaluateWithRuntime(
      runtime: RhoRuntime[IO],
      replayRuntime: ReplayRhoRuntime[IO]
  )(term: String, initialPhlo: Cost) = {
    implicit def rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
    for {
      // Save revert checkpoints
      startState       <- runtime.createSoftCheckpoint
      startReplayState <- replayRuntime.createSoftCheckpoint

      // Execute play
      playResult <- runtime.evaluate(term, initialPhlo, Map.empty, rand)

      // Create play snapshot (diff)
      playSnapshot              <- runtime.createSoftCheckpoint
      SoftCheckpoint(_, log, _) = playSnapshot

      // Prepare replay with events log from play
      _ <- replayRuntime.rig(log)

      // Execute replay
      replayResult <- replayRuntime
                       .evaluate(term, initialPhlo, Map.empty, rand)
                       .onError {
                         case _: Throwable =>
                           println(s"Executed term: $term")
                           println(s"Event log: $log").pure[IO]
                       }
      _ <- replayRuntime.checkReplayData.onError {
            case _: Throwable =>
              println(s"Executed term: $term")
              println(s"Event log: $log")
              println(s"Replay result: $replayResult").pure[IO]
          }

      // Revert all changes / reset to initial state
      _ <- runtime.revertToSoftCheckpoint(startState)
      _ <- replayRuntime.revertToSoftCheckpoint(startReplayState)
    } yield (playResult, replayResult)
  }

  def withRSpaceAndRuntime(op: (RhoRuntime[IO], ReplayRhoRuntime[IO]) => IO[Unit]) = {
    implicit val logF: Log[IO]           = new Log.NOPLog[IO]
    implicit val metricsEff: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
    implicit val noopSpan: Span[IO]      = NoopSpan[IO]()

    val resources = for {
      res <- Resources.mkRuntimes[IO]("cost-accounting-spec-")
    } yield res

    resources.use {
      case (runtime, replayRuntime, _) =>
        // Execute operation
        op(runtime, replayRuntime)
    }
  }

}
