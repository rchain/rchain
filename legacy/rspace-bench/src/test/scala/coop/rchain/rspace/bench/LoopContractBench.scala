package coop.rchain.rspace.bench

import java.util.concurrent.TimeUnit

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.rspace.bench.wide.WideBenchState
import org.openjdk.jmh.annotations.{
  Benchmark,
  BenchmarkMode,
  Fork,
  Measurement,
  Mode,
  OutputTimeUnit,
  Scope,
  State,
  Threads,
  Warmup
}
import org.openjdk.jmh.infra.Blackhole

class LoopContractBench {
  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def loopOne(bh: Blackhole, state: LoopOneContractBenchState): Unit =
    state.execute(bh)

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Threads(1)
  @Warmup(iterations = 2)
  @Measurement(iterations = 5)
  def loopTwo(bh: Blackhole, state: LoopTwoContractBenchState): Unit =
    state.execute(bh)
}

@State(Scope.Benchmark)
class LoopOneContractBenchState extends RhoBenchBaseState {
  override def testedRho: String =
    """
      |// Demonstrates a nested loop using a pattern that cleans up after itself
      |new loop1 in {
      |  match {
      |    for (@P1, @depth1 <- loop1) {
      |      if (depth1 == 0) {
      |        Nil
      |      } else {
      |        P1 |
      |        new loop2 in {
      |          match {
      |            for (@P2, @depth2 <- loop2) {
      |              if (depth2 == 0) {
      |                loop1!(P1, depth1 - 1)
      |              } else {
      |                P2 | loop2!(P2, depth2 - 1)
      |              }
      |            }
      |          } {
      |            P2 => { P2 | loop2!(P2, 140) }
      |          }
      |        }
      |      }
      |    }
      |  } {
      |    P1 => { P1 | loop1!(P1, 140) }
      |  }
      |}
      |""".stripMargin
}

@State(Scope.Benchmark)
class LoopTwoContractBenchState extends RhoBenchBaseState {
  override def testedRho: String =
    """
      |// Demonstrates the same nested loop as in linrec.rho, but using contracts
      |// where all closures have been eliminated and what used to be the environment
      |// is now passed in explicitly.
      |new loop1, loop2 in {
      |  contract loop1(@depth) = {
      |    if (depth == 0) {
      |      Nil
      |    } else {
      |      loop2!(depth, 140)
      |    }
      |  } |
      |  contract loop2(@depth1, @depth2) = {
      |    if (depth2 == 0) {
      |      loop1!(depth1 - 1)
      |    } else {
      |      loop2!(depth1, depth2 - 1)
      |    }
      |  } |
      |  loop1!(140)
      |}
      |""".stripMargin
}
