package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import coop.rchain.rspace.bench.EvalBenchStateBase
import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{Interpreter, Runtime}
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

class ReplayWideBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def wideReduce(bh: Blackhole, state: ReplayFineBenchState): Unit = {
    implicit val scheduler = state.scheduler
    val result             = state.runReplayTask.unsafeRunSync
    assert(result.isEmpty)
    bh.consume(result)
  }
}

@State(Scope.Benchmark)
class ReplayFineBenchState extends ReplayWideBenchState {
  override def createRuntime(): Runtime[Task] =
    Runtime.createWithEmptyCost[Task](dbDir, mapSize).unsafeRunSync
}

abstract class ReplayWideBenchState extends WideBenchBaseState {

  implicit val rand: Blake2b512Random        = Blake2b512Random(128)
  var runReplayTask: Task[Vector[Throwable]] = null

  @Setup(value = Level.Iteration)
  override def doSetup(): Unit = {
    super.doSetup()

    assert(createTest(setupTerm)(runtime.reducer, rand).unsafeRunSync.isEmpty)
    (for {
      setupCheckpoint <- runtime.space.createCheckpoint()
      _               <- runtime.replaySpace.rigAndReset(setupCheckpoint.root, setupCheckpoint.log)
      _               = assert(createTest(setupTerm)(runtime.reducer, rand).unsafeRunSync.isEmpty)
      runTask         = createTest(setupTerm)(runtime.reducer, rand)
      _               = assert(runTask.unsafeRunSync.isEmpty)
      checkpoint      <- runtime.space.createCheckpoint()
      _               <- runtime.replaySpace.rigAndReset(checkpoint.root, checkpoint.log)
      _               = runReplayTask = createTest(setupTerm)(runtime.reducer, rand)
    } yield ()).unsafeRunSync
  }
}
