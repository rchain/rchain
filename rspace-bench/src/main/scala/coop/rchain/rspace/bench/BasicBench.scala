package coop.rchain.rspace.bench

import java.nio.file.{Files, Path}

import cats.syntax.either._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.extended._
import coop.rchain.rspace.{LMDBStore, _}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

class BasicBench {

  import BasicBench._

  @Benchmark
  def consumeProduce(state: BenchState): Unit = {

    val space = state.testSpace

    space.consume(List("ch1", "ch2"),
                  List(StringMatch("bad"), StringMatch("finger")),
                  new StringsCaptor,
                  false)

    val r1 = space.produce("ch1", "bad", false)

    assert(r1.isEmpty)

    val r2 = space.produce("ch2", "finger", false)

    runK(r2)

    assert(getK(r2).results.head.toSet == Set("bad", "finger"))
  }
}

object BasicBench {

  @State(Scope.Benchmark)
  class BenchState {

    private val dbDir: Path = Files.createTempDirectory("rchain-storage-test-")

    val testStore: LMDBStore[String, Pattern, String, StringsCaptor] =
      LMDBStore.create[String, Pattern, String, StringsCaptor](dbDir, 1024 * 1024 * 1024)

    val testSpace: RSpace[String, Pattern, String, StringsCaptor] =
      new RSpace(testStore)
  }
}
