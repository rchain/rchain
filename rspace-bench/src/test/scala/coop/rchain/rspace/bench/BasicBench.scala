package coop.rchain.rspace.bench

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import cats.effect.{ContextShift, Sync}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.rholang.interpreter.accounting.Cost
import org.openjdk.jmh.annotations.{State => _, _}
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.openjdk.jmh.infra.Blackhole
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

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
        )(state.matcher)
        .unsafeRunSync

      assert(c1.right.get.isEmpty)
      bh.consume(c1)

      val r2 =
        space.produce(state.channels(i), state.data(i), false)(state.matcher).unsafeRunSync

      assert(r2.right.get.nonEmpty)
      bh.consume(r2)
      if (state.debug) {
        assert(space.store.isEmpty)
      }
    }
    if (state.debug) {
      assert(space.createCheckpoint().unsafeRunSync.log.size == 303)
    }
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def produceConsume(bh: Blackhole, state: BenchState): Unit = {
    val space = state.testSpace
    for (i <- 0 to 100) {
      val r2 =
        space.produce(state.channels(i), state.data(i), false)(state.matcher).unsafeRunSync

      assert(r2.right.get.isEmpty)
      bh.consume(r2)

      val c1 = space
        .consume(
          state.channels(i) :: Nil,
          state.patterns(i) :: Nil,
          state.tc.head,
          false
        )(state.matcher)
        .unsafeRunSync

      assert(c1.right.get.nonEmpty)
      bh.consume(c1)
      if (state.debug) {
        assert(space.store.isEmpty)
      }
    }
    if (state.debug) {
      assert(space.createCheckpoint().unsafeRunSync.log.size == 303)
    }
  }
}

object BasicBench {

  @org.openjdk.jmh.annotations.State(Scope.Benchmark)
  class BenchState {
    val debug: Boolean = false

    import coop.rchain.rholang.interpreter.storage.implicits._

    implicit val syncF: Sync[Task]                 = Task.catsEffect
    implicit val contextShiftF: ContextShift[Task] = Task.contextShift

    private val dbDir: Path = Files.createTempDirectory("rchain-storage-test-")

    val context: LMDBContext[Par, BindPattern, ListParWithRandom, TaggedContinuation] =
      Context.create(dbDir, 1024L * 1024L * 1024L)

    val testStore: LMDBStore[Par, BindPattern, ListParWithRandom, TaggedContinuation] =
      LMDBStore.create[Par, BindPattern, ListParWithRandom, TaggedContinuation](
        context,
        Branch("bench")
      )

    val testSpace: ISpace[
      Task,
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ] =
      RSpace
        .create[
          Task,
          Par,
          BindPattern,
          OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos,
          TaggedContinuation
        ](
          testStore,
          Branch("bench")
        )
        .unsafeRunSync

    implicit val matcher = matchListPar(Cost(10000000L))

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
        } yield GInt(v)
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
        } yield
          (
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
        } yield
          TaggedContinuation(
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
    def tearDown(): Unit = {
      testSpace.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}
