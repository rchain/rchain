package coop.rchain.models.rholangn

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import scala.util.Random

@Fork(value = 1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OperationsPerInvocation(value = 100)
@State(Scope.Benchmark)
class SetBench {

  final def setCreation(n: Int): ESetN =
    (1 to n).foldLeft(ESetN()) { (acc, _) =>
      acc + GIntN(Random.nextLong())
    }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def manyAppends(): Unit = {
    val _ = setCreation(5000)
  }
}
