package coop.rchain.storage.bench

import java.nio.file.{Files, Path}

import cats.syntax.either._
import coop.rchain.storage.examples.StringExamples._
import coop.rchain.storage.examples.StringExamples.implicits._
import coop.rchain.storage.{LMDBStore, _}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

class BasicBench {

  import BasicBench._

  @Benchmark
  def consumeProduce(state: BenchState): Unit = {

    consume(state.testStore,
            List("ch1", "ch2"),
            List(StringMatch("bad"), StringMatch("finger")),
            new StringsCaptor)

    val r1 = produce(state.testStore, "ch1", "bad")

    assert(r1.isEmpty)

    val r2 = produce(state.testStore, "ch2", "finger")

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
  }
}
