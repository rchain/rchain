package coop.rchain.lmdb

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.lmdb.LMDBOpsBench._
import coop.rchain.rspace.Context
import coop.rchain.shared.PathOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

class LMDBOpsBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def smallBlobRoundtrip(bh: Blackhole, state: SmallLMDBOpsBenchState) = {
    val ress = state.roundtrip(state.small, state.dbGnats, state.env)
    assert(ress == state.small)
    bh.consume(ress)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def smallBlobPut(bh: Blackhole, state: SmallLMDBOpsBenchState) =
    state.small.foreach(x => {
      val txn = state.env.txnWrite()
      state.dbGnats.put(txn, x._1, x._2)
      txn.commit()
      txn.close()
    })

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def smallBlobGet(bh: Blackhole, state: SmallPrefilledLMDBOpsBenchState) = {
    val res = state.small.map(x => {
      val txn = state.env.txnRead()
      val r   = (x._1, state.dbGnats.get(txn, x._1))
      txn.commit()
      txn.close()
      r
    })
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def mediumBlobRoundtrip(bh: Blackhole, state: MediumLMDBOpsBenchState) = {
    val ress = state.roundtrip(state.medium, state.dbGnats, state.env)
    assert(ress == state.medium)
    bh.consume(ress)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def mediumBlobPut(bh: Blackhole, state: MediumLMDBOpsBenchState) =
    state.medium.foreach(x => {
      val txn = state.env.txnWrite()
      state.dbGnats.put(txn, x._1, x._2)
      txn.commit()
      txn.close()
    })

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def mediumBlobGet(bh: Blackhole, state: MediumPrefilledLMDBOpsBenchState) = {
    val res = state.medium.map(x => {
      val txn = state.env.txnRead()
      val r   = (x._1, state.dbGnats.get(txn, x._1))
      txn.commit()
      txn.close()
      r
    })
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def largeBlobRoundtrip(bh: Blackhole, state: LargeLMDBOpsBenchState) = {
    val ress = state.roundtrip(state.large, state.dbGnats, state.env)
    assert(ress == state.large)
    bh.consume(ress)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def largeBlobPut(bh: Blackhole, state: LargeLMDBOpsBenchState) =
    state.large.foreach(x => {
      val txn = state.env.txnWrite()
      state.dbGnats.put(txn, x._1, x._2)
      txn.commit()
      txn.close()
    })

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Fork(value = 1)
  @Measurement(iterations = 10)
  def largeBlobGet(bh: Blackhole, state: LargePrefilledLMDBOpsBenchState) = {
    val res = state.large.map(x => {
      val txn = state.env.txnRead()
      val r   = (x._1, state.dbGnats.get(txn, x._1))
      txn.commit()
      txn.close()
      r
    })
    bh.consume(res)
  }
}

object LMDBOpsBench {
  @State(Scope.Benchmark)
  class SmallLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var small: Seq[(ByteBuffer, ByteBuffer)] = _

    @Setup(value = Level.Iteration)
    override def setup() = {
      super.setup()
      small = generate(10000, 10)
    }
  }

  @State(Scope.Benchmark)
  class SmallPrefilledLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var small: Seq[(ByteBuffer, ByteBuffer)] = generate(10000, 10)

    @Setup(value = Level.Iteration)
    override def setup() = {
      super.setup()
      small.foreach(x => dbGnats.put(x._1, x._2))
    }
  }

  @State(Scope.Benchmark)
  class MediumLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var medium: Seq[(ByteBuffer, ByteBuffer)] = generate(1000, 100)

    @Setup(value = Level.Iteration)
    override def setup() =
      super.setup()
  }

  @State(Scope.Benchmark)
  class MediumPrefilledLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var medium: Seq[(ByteBuffer, ByteBuffer)] = generate(1000, 100)

    @Setup(value = Level.Iteration)
    override def setup() = {
      super.setup()
      medium.foreach(x => dbGnats.put(x._1, x._2))
    }
  }

  @State(Scope.Benchmark)
  class LargeLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var large: Seq[(ByteBuffer, ByteBuffer)] = generate(100, 1000)

    @Setup(value = Level.Iteration)
    override def setup() =
      super.setup()

  }

  @State(Scope.Benchmark)
  class LargePrefilledLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var large: Seq[(ByteBuffer, ByteBuffer)] = generate(100, 1000)

    @Setup(value = Level.Iteration)
    override def setup() = {
      super.setup()
      large.foreach(x => dbGnats.put(x._1, x._2))
    }
  }

  @State(Scope.Benchmark)
  class KeyRangeLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var keyRange: Seq[(ByteBuffer, ByteBuffer)] =
      (0 to 5).map(i => (indexedKey(i), hash(100)))

    @Setup(value = Level.Iteration)
    override def setup() =
      super.setup()
  }

  trait BaseLMDBOpsBenchState {

    val mapSize: Long = 1024L * 1024L * 1024L

    var dbDir: Path              = _
    var dbGnats: Dbi[ByteBuffer] = _
    var env: Env[ByteBuffer]     = _

    val randomData = generate(100, 100)

    def indexedKey(i: Int): ByteBuffer = {
      val bytes = Array.fill[Byte](2)(Random.nextInt().toByte)
      val mbb   = ByteBuffer.allocateDirect(bytes.length + 4)
      mbb.put(bytes)
      mbb.putInt(i)
      mbb.flip()
      mbb
    }

    def populate(): Unit = {
      val txn = env.txnWrite()
      randomData.foreach(x => {
        dbGnats.put(txn, x._1, x._2)
      })
      txn.commit()
      txn.close()
    }

    def setup(): Unit = {
      dbDir = Files.createTempDirectory("rchain-rspace-mixed-bench-")
      val flags = List(EnvFlags.MDB_NOTLS, EnvFlags.MDB_NORDAHEAD)
      env = Context.env(dbDir, mapSize, flags)
      dbGnats = env.openDbi(s"db-gnats", MDB_CREATE)
      populate()
    }

    def generate(elems: Int, hashSize: Int): Seq[(ByteBuffer, ByteBuffer)] =
      (0 to elems).map(_ => (key, hash(hashSize)))

    def roundtrip(
        data: Seq[(ByteBuffer, ByteBuffer)],
        db: Dbi[ByteBuffer],
        env: Env[ByteBuffer]
    ): Seq[(ByteBuffer, ByteBuffer)] = {
      data.foreach(x => {
        val txn = env.txnWrite()
        db.put(txn, x._1, x._2)
        txn.commit()
        txn.close()
      })
      data.map(x => {
        val txn = env.txnRead()
        val r   = (x._1, db.get(txn, x._1))
        txn.commit()
        txn.close()
        r
      })
    }

    def key: ByteBuffer = {
      val bytes = Array.fill[Byte](32)(Random.nextInt().toByte)
      val bb    = ByteBuffer.allocateDirect(bytes.length)
      bb.put(bytes)
      bb.flip()
      bb
    }

    def hash(s: Int): ByteBuffer = {
      val size: Int = 1024 * s * 10
      val bytes     = Array.fill[Byte](size)(Random.nextInt().toByte)
      val bb        = ByteBuffer.allocateDirect(bytes.length)
      bb.put(bytes)
    }

    @TearDown
    def tearDown() = {
      dbDir.recursivelyDelete()
      ()
    }
  }

}
