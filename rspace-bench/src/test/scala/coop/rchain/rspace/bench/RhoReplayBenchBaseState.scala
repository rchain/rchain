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
    assert(r.isEmpty)
    bh.consume(processErrors(r))
  }

  @Setup(value = Level.Iteration)
  override def doSetup(): Unit = {
    super.doSetup()

    assert(runTask.unsafeRunSync.isEmpty)
    (for {
      executionCheckpoint <- runtime.space.createCheckpoint()
      _                   <- runtime.replaySpace.rigAndReset(executionCheckpoint.root, executionCheckpoint.log)
      _ = assert(
        createTest(setupTerm)(readErrors, runtime.replayReducer, randSetup).unsafeRunSync.isEmpty
      )
      _ = runTask = createTest(Some(term))(readErrors, runtime.replayReducer, randRun)
    } yield ()).unsafeRunSync
  }
}
