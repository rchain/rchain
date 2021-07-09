package coop.rchain.rspace.hashing

import java.util.concurrent.TimeUnit

import scala.collection.immutable.Seq

import coop.rchain.models._
import coop.rchain.models.testImplicits._
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rspace._
import coop.rchain.rspace.bench._
import coop.rchain.rspace.trace.Consume
import org.openjdk.jmh.annotations.{State => BenchState, _}
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

@Fork(value = 1)
@Warmup(iterations = 1)
@Measurement(iterations = 10)
@OperationsPerInvocation(value = 100)
@BenchState(Scope.Benchmark)
class TraceEventCreationBench {

  def arbitraryConsumeData[C, P, K](
      implicit
      arbC: Arbitrary[C],
      arbP: Arbitrary[P],
      arbK: Arbitrary[K]
  ): Arbitrary[(Seq[C], Seq[P], K)] =
    Arbitrary(Gen.sized { size =>
      val constrainedSize = Math.max(1, size)
      for {
        chans        <- Gen.containerOfN[List, C](constrainedSize, arbC.arbitrary)
        pats         <- Gen.containerOfN[List, P](chans.length, arbP.arbitrary)
        continuation <- arbK.arbitrary
      } yield (chans, pats, continuation)
    })

  val light =
    generateSeq[(Seq[Par], Seq[BindPattern], ListParWithRandom)](1)(
      arbitraryConsumeData[Par, BindPattern, ListParWithRandom]
    )
  val medium =
    generateSeq[(Seq[Par], Seq[BindPattern], ListParWithRandom)](5)(
      arbitraryConsumeData[Par, BindPattern, ListParWithRandom]
    )
  val heavy =
    generateSeq[(Seq[Par], Seq[BindPattern], ListParWithRandom)](10)(
      arbitraryConsumeData[Par, BindPattern, ListParWithRandom]
    )

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def lightConsume(bh: Blackhole): Unit =
    doBench(bh, light)

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def mediumConsume(bh: Blackhole): Unit =
    doBench(bh, medium)

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def heavyConsume(bh: Blackhole): Unit =
    doBench(bh, heavy)

  @inline
  def doBench(bh: Blackhole, seq: Seq[(Seq[Par], Seq[BindPattern], ListParWithRandom)]): Unit =
    seq.map {
      case (channels, patterns, continuation) =>
        bh.consume(Consume(channels, patterns, continuation, false))
    }
}
