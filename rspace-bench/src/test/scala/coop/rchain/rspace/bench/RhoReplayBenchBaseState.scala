package coop.rchain.rspace.bench

import coop.rchain.catscontrib.TaskContrib._
import org.openjdk.jmh.annotations.{Level, Setup}
import org.openjdk.jmh.infra.Blackhole

abstract class RhoReplayBenchBaseState extends RhoBenchBaseState {

  override def execute(bh: Blackhole): Unit = {
    val r = (for {
      result <- runTask
      _      <- runtime.replaySpace.createCheckpoint()
    } yield result).unsafeRunSync
    bh.consume(r)
  }

  @Setup(value = Level.Iteration)
  override def doSetup(): Unit = {
    super.doSetup()

    runTask.unsafeRunSync
    (for {
      executionCheckpoint <- runtime.space.createCheckpoint()
      _                   <- runtime.replaySpace.rigAndReset(executionCheckpoint.root, executionCheckpoint.log)
      _                   <- createTest(setupTerm)(runtime.replayReducer, randSetup)
      _                   = runTask = createTest(Some(term))(runtime.replayReducer, randRun)
    } yield ()).unsafeRunSync
  }
}
