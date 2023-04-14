package coop.rchain.rspace.bench

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RholangCLI
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rspace.{Match, RSpace, _}
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps.RichPath
import org.openjdk.jmh.annotations.{State => _, _}
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.collection.immutable.{BitSet, Seq}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1)
@Warmup(iterations = 1)
@Measurement(iterations = 10)
@OperationsPerInvocation(value = 100)
class BasicBench {

  import BasicBench._

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def consumeProduce(bh: Blackhole, state: BenchState): Unit = {
    val space = state.testSpace
    for (i <- 0 to 100) {
      val c1 = space
        .consume(
          state.channels(i) :: Nil,
          state.patterns(i) :: Nil,
          state.tc.head,
          false
        )
        .unsafeRunSync()

      assert(c1.isEmpty)
      bh.consume(c1)

      val r2 =
        space.produce(state.channels(i), state.data(i), false).unsafeRunSync()

      assert(r2.nonEmpty)
      bh.consume(r2)
      if (state.debug) {
        assert(space.toMap.unsafeRunSync().isEmpty)
      }
    }
    if (state.debug) {
      assert(space.createCheckpoint().unsafeRunSync().log.size == 303)
    }
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def produceConsume(bh: Blackhole, state: BenchState): Unit = {
    val space = state.testSpace
    for (i <- 0 to 100) {
      val r2 =
        space.produce(state.channels(i), state.data(i), false).unsafeRunSync

      assert(r2.isEmpty)
      bh.consume(r2)

      val c1 = space
        .consume(
          state.channels(i) :: Nil,
          state.patterns(i) :: Nil,
          state.tc.head,
          false
        )
        .unsafeRunSync()

      assert(c1.nonEmpty)
      bh.consume(c1)
      if (state.debug) {
        assert(space.toMap.unsafeRunSync().isEmpty)
      }
    }
    if (state.debug) {
      assert(space.createCheckpoint().unsafeRunSync().log.size == 303)
    }
  }
}

object BasicBench {

  @org.openjdk.jmh.annotations.State(Scope.Benchmark)
  class BenchState {
    val debug: Boolean = false

    import coop.rchain.rholang.interpreter.storage._
    implicit val logF: Log[IO]                                = new Log.NOPLog[IO]
    implicit val noopMetrics: Metrics[IO]                     = new metrics.Metrics.MetricsNOP[IO]
    implicit val noopSpan: Span[IO]                           = NoopSpan[IO]()
    implicit val m: Match[IO, BindPattern, ListParWithRandom] = matchListPar[IO]
    implicit val ms: Metrics.Source                           = Metrics.BaseSource
    private val dbDir: Path                                   = Files.createTempDirectory("rchain-storage-test-")
    implicit val kvm                                          = RholangCLI.mkRSpaceStoreManager[IO](dbDir).unsafeRunSync()
    val rSpaceStore                                           = kvm.rSpaceStores.unsafeRunSync()

    val testSpace: ISpace[
      IO,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ] =
      RSpace
        .create[
          IO,
          Par,
          BindPattern,
          ListParWithRandom,
          TaggedContinuation
        ](rSpaceStore)
        .unsafeRunSync()
    implicit val cost = CostAccounting.initialCost[IO](Cost.UNSAFE_MAX).unsafeRunSync()

    val initSeed = 123456789L

    def generate[A: Arbitrary](size: Int = 1): Seq[A] = {
      val params = Parameters.default.withSize(1000)
      (1 to size).map(
        i => implicitly[Arbitrary[A]].arbitrary.apply(params, Seed(initSeed + i)).get
      )
    }

    val arbitraryGInt: Arbitrary[GInt] =
      Arbitrary(
        for {
          v <- Gen.posNum[Int]
        } yield GInt(v.toLong)
      )

    def onePar(i: GInt) = List(
      Par(
        List(),
        List(),
        List(),
        List(Expr(i)),
        List(),
        List(),
        List(),
        List(),
        AlwaysEqual(BitSet()),
        false
      )
    )

    val arbitraryDataAndPattern: Arbitrary[(ListParWithRandom, BindPattern)] =
      Arbitrary(
        for {
          i <- arbitraryGInt.arbitrary
          r <- Blake2b512Random.arbitrary.arbitrary
        } yield (
          ListParWithRandom(
            onePar(i),
            r
          ),
          BindPattern(
            onePar(i),
            None,
            0
          )
        )
      )

    val arbitraryChannel: Arbitrary[Par] =
      Arbitrary(
        for {
          i <- arbitraryGInt.arbitrary
        } yield onePar(i).head
      )

    val arbitraryContinuation: Arbitrary[TaggedContinuation] =
      Arbitrary(
        for {
          r <- Blake2b512Random.arbitrary.arbitrary
        } yield TaggedContinuation(
          ParBody(
            ParWithRandom(
              Par(
                Vector(
                  Send(
                    Par(
                      Vector(),
                      Vector(),
                      Vector(),
                      List(Expr(GInt(2))),
                      Vector(),
                      Vector(),
                      Vector(),
                      List(),
                      AlwaysEqual(BitSet()),
                      false
                    ),
                    Vector(
                      Par(
                        Vector(),
                        Vector(),
                        Vector(),
                        List(Expr(GString("OK"))),
                        Vector(),
                        Vector(),
                        Vector(),
                        List(),
                        AlwaysEqual(BitSet()),
                        false
                      )
                    ),
                    false,
                    AlwaysEqual(BitSet()),
                    false
                  )
                ),
                Vector(),
                Vector(),
                List(),
                Vector(),
                Vector(),
                Vector(),
                List(),
                AlwaysEqual(BitSet()),
                false
              ),
              r
            )
          )
        )
      )

    val channels: Vector[Par] = generate[Par](1000)(arbitraryChannel).toVector
    val (data, patterns): (Vector[ListParWithRandom], Vector[BindPattern]) =
      generate[(ListParWithRandom, BindPattern)](1000)(arbitraryDataAndPattern).toVector.unzip
    val tc: Vector[TaggedContinuation] =
      generate[TaggedContinuation]()(arbitraryContinuation).toVector

    @TearDown
    def tearDown(): Unit =
      dbDir.recursivelyDelete()
  }
}
