package coop.rchain.rspace.bench.wide

import java.util.concurrent.TimeUnit

import coop.rchain.rspace.bench._
import coop.rchain.rspace.bench.RhoBenchBaseState
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class WideBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def wideReduce(bh: Blackhole, state: WideBenchState): Unit =
    state.execute(bh)
}

@State(Scope.Benchmark)
class WideBenchState extends RhoBenchBaseState {
  override def setupRho: Option[String] = Some(resourceFileReader("/rholang/wide-setup.rho"))
  override def testedRho: String        = resourceFileReader("/rholang/wide.rho")
}
