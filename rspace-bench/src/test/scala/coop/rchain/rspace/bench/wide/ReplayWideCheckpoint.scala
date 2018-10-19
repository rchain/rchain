package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import coop.rchain.rholang.interpreter.ChargingReducer
import coop.rchain.rspace.bench.EvalBenchStateBase
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import coop.rchain.shared.StoreType
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

class ReplayWideCheckpoint {
  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 0)
  @Measurement(iterations = 1)
  def wideReduce(bh: Blackhole, state: ReplayWideCheckpointState): Unit = {
    println("begin wideReduce()")
    val checkpoint = state.runtime.space.createCheckpoint()
    //state.runtime.replaySpace.rig(checkpoint.root, checkpoint.log)
    println("end wideReduce()")
  }
}

@State(Scope.Benchmark)
class ReplayWideCheckpointState extends WideBenchBaseState {

  implicit val rand: Blake2b512Random        = Blake2b512Random(128)
  var runReplayTask: Task[Vector[Throwable]] = Task.now(Vector.empty)

  @Setup(value = Level.Iteration)
  override def doSetup(): Unit = {
    super.doSetup()

    assert(createTest(setupTerm, runtime.reducer).unsafeRunSync.isEmpty)
    val setupCheckpoint = runtime.space.createCheckpoint()
    runtime.replaySpace.rig(setupCheckpoint.root, setupCheckpoint.log)
    assert(createTest(setupTerm, runtime.replayReducer).unsafeRunSync.isEmpty)
    val runTask = createTest(term, runtime.reducer)
    assert(runTask.unsafeRunSync.isEmpty)

    println("end ReplayWideBenchState setup")
  }
}