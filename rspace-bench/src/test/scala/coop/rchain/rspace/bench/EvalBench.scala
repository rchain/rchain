package coop.rchain.rspace.bench

import cats.effect.IO
import monix.execution.schedulers.{CanBlock, TrampolineScheduler}
import monix.execution.{ExecutionModel, Scheduler}
import org.openjdk.jmh.annotations._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

//for debug/check run from IDEA with
//rspaceBench/jmh:run EvalBench.reduceMVCEPPST -i 1 -wi 0 -f 0 -t 1
//on hyper-threaded machines for standalone check use -f <number of physical cores>
//for example
//java -jar target/scala-2.12/rspacebench_2.12-0.1.0-SNAPSHOT.jar EvalBench -i 10 -wi 5 -f 2 -t 2
class EvalBench {
// TODO enable back
//  import EvalBench._
//
//  def createTest(state: EvalBenchStateBase): IO[Unit] = {
//    val par = state.term.getOrElse(throw new Error("Failed to prepare executable rholang term"))
//    state.runtime.inj(par)(state.rand)
//  }
//
//  //if we run multiple tests on a single-threaded scheduler
//  //they will compete on single execution queue
//  //therefore this test always limited to one thread
//  @Benchmark
//  @Threads(1)
//  def reduceMVCEPPST(state: MVCEPPBenchState): Unit = {
//    val runIO = createTest(state).executeOn(state.singleThreadedScheduler, forceAsync = false)
//    runIO.runSyncUnsafe(Duration.Inf)(state.singleThreadedScheduler, CanBlock.permit)
//  }
//
//  @Benchmark
//  def reduceMVCEPPMT(state: MVCEPPBenchState): Unit = {
//    implicit val scheduler: Scheduler = monix.execution.Scheduler.Implicits.global
//    val runIO                         = createTest(state)
//    Await.result(runIO.runToFuture, Duration.Inf)
//  }
}

object EvalBench {

  @State(Scope.Benchmark)
  class MVCEPPBenchState extends EvalBenchStateBase {
    val singleThreadedScheduler: Scheduler = TrampolineScheduler.apply(
      Scheduler.singleThread(name = "mvcepp-1"),
      ExecutionModel.SynchronousExecution
    )

    override val rhoScriptSource: String = "/rholang/mvcepp.rho"
  }
}
