package coop.rchain.rspace.bench

import coop.rchain.crypto.Blake2b256Hash

import java.util.concurrent.TimeUnit
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.ByteVectorOps._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scodec.Codec

import scala.language.postfixOps
import scala.util.Random

class KeyBench {
  import KeyBench._

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def prepareKeyUsingCodec(blackhole: Blackhole, bs: KeyState): Unit =
    bs.hashes.foreach { h =>
      blackhole.consume(Codec[Blake2b256Hash].encode(h).get.bytes.toDirectByteBuffer)
    }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def prepareKeyUsingRawBlakeHash(blackhole: Blackhole, bs: KeyState): Unit =
    bs.hashes.foreach { h =>
      blackhole.consume(h.bytes.toDirectByteBuffer)
    }
}

object KeyBench {
  @State(Scope.Benchmark)
  class KeyState {
    def hash: Blake2b256Hash = {
      val bytes = Array.fill[Byte](32)(Random.nextInt().toByte)
      Blake2b256Hash.create(bytes)
    }

    val hashes: List[Blake2b256Hash] =
      0 to 1000 map { _ =>
        this.hash
      } toList
  }
}
