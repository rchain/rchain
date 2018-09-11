package coop.rchain.rspace.bench
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import monix.execution.schedulers.TrampolineScheduler
import org.openjdk.jmh.annotations.{Benchmark, Scope, State, Threads}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class WideBench {

  import WideBench._

  def processErrors(errors: Vector[Throwable]): Unit = if (errors.nonEmpty) {
    throw new RuntimeException(
      errors
        .map(_.toString())
        .mkString("Errors received during evaluation:\n", "\n", "\n"))
  }

  def createTest(state: EvalBenchStateBase): Task[Vector[Throwable]] = {
    val par = state.term.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    state.runtime.reducer
      .inj(par)(state.rand, state.costAccountAlg)
      .map(_ => state.runtime.readAndClearErrorVector())
  }

  @Benchmark
  @Threads(8)
  def wideReduce(state: WideBenchState): Unit = {
    implicit val scheduler: Scheduler =
      TrampolineScheduler.apply(Scheduler.fixedPool(name = "wide-1", poolSize = 8),
                                ExecutionModel.AlwaysAsyncExecution)
    val runTask = createTest(state)
    processErrors(Await.result(runTask.runAsync, Duration.Inf))
  }
}

object WideBench {

  @State(Scope.Benchmark)
  class WideBenchState extends EvalBenchStateBase {
    override val rhoScriptSource: String = "/rholang/wide.rho"
  }
}
