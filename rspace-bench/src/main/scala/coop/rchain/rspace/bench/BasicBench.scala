package coop.rchain.rspace.bench

import java.nio.file.{Files, Path}

import coop.rchain.rspace.ISpace.IdISpace
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.util._
import coop.rchain.rspace.{LMDBStore, _}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State, TearDown}
import coop.rchain.shared.PathOps.RichPath

class BasicBench {

  import BasicBench._

  @Benchmark
  def consumeProduce(state: BenchState): Unit = {

    val space = state.testSpace

    space
      .consume(
        List("ch1", "ch2"),
        List(StringMatch("bad"), StringMatch("finger")),
        new StringsCaptor,
        false
      )

    val r1 = space.produce("ch1", "bad", false)

    assert(r1.right.get.isEmpty)

    val r2 = space.produce("ch2", "finger", false)

    runK(r2)

    assert(getK(r2).results.head.toSet == Set("bad", "finger"))
  }

}

object BasicBench {

  @State(Scope.Benchmark)
  class BenchState {

    private val dbDir: Path = Files.createTempDirectory("rchain-storage-test-")

    val context: LMDBContext[String, Pattern, String, StringsCaptor] =
      Context.create(dbDir, 1024L * 1024L * 1024L)

    val testStore: LMDBStore[String, Pattern, String, StringsCaptor] =
      LMDBStore.create[String, Pattern, String, StringsCaptor](context, Branch("bench"))

    val testSpace: IdISpace[String, Pattern, Nothing, String, String, StringsCaptor] =
      RSpace.create[String, Pattern, Nothing, String, String, StringsCaptor](
        testStore,
        Branch("bench")
      )

    @TearDown
    def tearDown(): Unit = {
      testSpace.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}
