package coop.rchain.rholang.interpreter

import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost}
import coop.rchain.rspace.SoftCheckpoint
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class ReplaySpec extends FlatSpec with Matchers {

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
        |  x!() | for(<- x; <- x) { 0 } | x!()
        |}
        |""".stripMargin
    testRholangTerm(term, 500, 30.seconds)
  }

  "multi joins (4/2)" should "execute successfully in replay" ignore {
    val term =
      """
        |new x in {
        |  x!() | x!() | for(<- x; <- x) { 0 } | x!() | x!()
        |}
        |""".stripMargin
    testRholangTerm(term, 500, 30.seconds)
  }

  def testRholangTerm(term: String, iterations: Int, timeout: Duration) =
    withRSpaceAndRuntime { runtime =>
      for (i <- 1 to iterations) {
        val (playRes, replayRes) = evaluateWithRuntime(runtime)(term, Cost(Integer.MAX_VALUE))
          .onError {
            case _: Throwable =>
              println(s"Test retry count: $i").pure[Task]
          }
          .runSyncUnsafe(1.seconds)

        assert(playRes.errors.isEmpty)
        assert(replayRes.errors.isEmpty)
      }
      ().pure[Task]
    }.runSyncUnsafe(timeout)

  def evaluateWithRuntime(runtime: Runtime[Task])(term: String, initialPhlo: Cost) = {
    implicit val c: _cost[Task]         = runtime.cost
    implicit def rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
    for {
      // Save revert checkpoints
      startState       <- runtime.space.createSoftCheckpoint()
      startReplayState <- runtime.replaySpace.createSoftCheckpoint()

      // Execute play
      playResult <- Interpreter[Task]
                     .injAttempt(
                       runtime.reducer,
                       term,
                       initialPhlo,
                       Map.empty
                     )(rand)

      // Create play snapshot (diff)
      playSnapshot              <- runtime.space.createSoftCheckpoint()
      SoftCheckpoint(_, log, _) = playSnapshot

      // Prepare replay with events log from play
      _ <- runtime.replaySpace.rig(log)

      // Execute replay
      replayResult <- Interpreter[Task]
                       .injAttempt(
                         runtime.replayReducer,
                         term,
                         initialPhlo,
                         Map.empty
                       )(rand)
                       .onError {
                         case _: Throwable =>
                           println(s"Executed term: $term")
                           println(s"Event log: $log").pure[Task]
                       }
      _ <- runtime.replaySpace.checkReplayData().onError {
            case _: Throwable =>
              println(s"Executed term: $term")
              println(s"Event log: $log")
              println(s"Replay result: $replayResult").pure[Task]
          }

      // Revert all changes / reset to initial state
      _ <- runtime.space.revertToSoftCheckpoint(startState)
      _ <- runtime.replaySpace.revertToSoftCheckpoint(startReplayState)
    } yield (playResult, replayResult)
  }

  def withRSpaceAndRuntime(op: Runtime[Task] => Task[Unit]) = {
    implicit val logF: Log[Task]           = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val kvm                       = InMemoryStoreManager[Task]

    // Execute operation
    Resources.mkRuntimeAt(kvm).map(_._1).flatMap(op)
  }

}
