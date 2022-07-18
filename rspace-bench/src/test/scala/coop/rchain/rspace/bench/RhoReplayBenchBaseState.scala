package coop.rchain.rspace.bench

import org.openjdk.jmh.annotations.{Level, Setup}
import org.openjdk.jmh.infra.Blackhole

abstract class RhoReplayBenchBaseState extends RhoBenchBaseState {

  override def execute(bh: Blackhole): Unit = {
    val r = (for {
      result <- runTask
      _      <- replayRuntime.createCheckpoint
    } yield result).runSyncUnsafe()
    bh.consume(r)
  }

  @Setup(value = Level.Iteration)
  override def doSetup(): Unit = {
    super.doSetup()

    runTask.runSyncUnsafe()
    (for {
      executionCheckpoint <- replayRuntime.createCheckpoint
      _                   <- replayRuntime.rig(executionCheckpoint.log)
      _                   <- replayRuntime.reset(executionCheckpoint.root)
      _                   <- createTest(setupTerm)(replayRuntime, randSetup)
      _                   = runTask = createTest(Some(term))(replayRuntime, randRun)
    } yield ()).runSyncUnsafe()
  }
}
