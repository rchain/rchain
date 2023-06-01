package coop.rchain.rspace.bench

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.openjdk.jmh.annotations._

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

//for debug/check run from IDEA with
//rspaceBench/jmh:run EvalBench.reduceMVCEPPST -i 1 -wi 0 -f 0 -t 1
//on hyper-threaded machines for standalone check use -f <number of physical cores>
//for example
//java -jar target/scala-2.12/rspacebench_2.12-0.1.0-SNAPSHOT.jar EvalBench -i 10 -wi 5 -f 2 -t 2
class EvalBench {
  import EvalBench._

  def createTest(state: EvalBenchStateBase): IO[Unit] = {
    val par = state.term.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    state.runtime.inj(par)(state.rand)
  }

  //if we run multiple tests on a single-threaded scheduler
  //they will compete on single execution queue
  //therefore this test always limited to one thread
  @Benchmark
  @Threads(1)
  def reduceMVCEPPST(state: MVCEPPBenchState): Unit = {
    val runIO = createTest(state)
    import state.singleThreadedScheduler
    runIO.unsafeRunSync()
  }

  @Benchmark
  def reduceMVCEPPMT(state: MVCEPPBenchState): Unit = {
    val runIO = createTest(state)
    import state.singleThreadedScheduler
    Await.result(runIO.unsafeToFuture(), Duration.Inf)
  }
}

object EvalBench {

  @State(Scope.Benchmark)
  class MVCEPPBenchState extends EvalBenchStateBase {
    implicit val singleThreadedScheduler: IORuntime = {
      val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadScheduledExecutor())
      IORuntime.builder().setCompute(ec, () => ())
    }.build()

    override val rhoScriptSource: String = "/rholang/mvcepp.rho"
  }
}
