package coop.rchain.rspace.bench.wide

import coop.rchain.rspace.bench._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class ReplayWideBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def wideReduce(bh: Blackhole, state: ReplayWideBenchState): Unit =
    state.execute(bh)
}

@State(Scope.Benchmark)
class ReplayWideBenchState extends RhoReplayBenchBaseState {
  override def setupRho: Option[String] = Some(resourceFileReader("/rholang/wide-setup.rho"))
  override def testedRho: String        = resourceFileReader("/rholang/wide.rho")
}
