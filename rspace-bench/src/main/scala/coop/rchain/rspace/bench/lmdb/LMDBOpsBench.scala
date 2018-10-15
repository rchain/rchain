package coop.rchain.rspace.bench.lmdb
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import coop.rchain.rspace.bench.lmdb.LMDBOpsBench._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.examples.AddressBookExample.{Channel, EntriesCaptor, Entry, Pattern}
import coop.rchain.rspace.history.{Branch, LMDBTrieStore}
import coop.rchain.rspace.internal.GNAT
import coop.rchain.rspace.{Blake2b256Hash, Context, LMDBStore}
import coop.rchain.shared.PathOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scodec.Codec
import scala.collection.JavaConverters._

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

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
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

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
    }
  }
  @State(Scope.Benchmark)
  class MediumLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var medium: Seq[(ByteBuffer, ByteBuffer)] = generate(1000, 100)

    @Setup(value = Level.Iteration)
    override def setup() =
      super.setup()

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
    }
  }

  @State(Scope.Benchmark)
  class MediumPrefilledLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var medium: Seq[(ByteBuffer, ByteBuffer)] = generate(1000, 100)

    @Setup(value = Level.Iteration)
    override def setup() = {
      super.setup()
      medium.foreach(x => dbGnats.put(x._1, x._2))
    }

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
    }
  }

  @State(Scope.Benchmark)
  class LargeLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var large: Seq[(ByteBuffer, ByteBuffer)] = generate(100, 1000)

    @Setup(value = Level.Iteration)
    override def setup() =
      super.setup()

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
    }
  }

  @State(Scope.Benchmark)
  class LargePrefilledLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var large: Seq[(ByteBuffer, ByteBuffer)] = generate(100, 1000)

    @Setup(value = Level.Iteration)
    override def setup() = {
      super.setup()
      large.foreach(x => dbGnats.put(x._1, x._2))
    }

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
    }
  }

  @State(Scope.Benchmark)
  class KeyRangeLMDBOpsBenchState extends BaseLMDBOpsBenchState {

    var keyRange: Seq[(ByteBuffer, ByteBuffer)] =
      (0 to 5).map(i => (indexedKey(i), hash(100)))

    @Setup(value = Level.Iteration)
    override def setup() =
      super.setup()

    @TearDown
    def tearDown() = {
      store.close()
      dbDir.recursivelyDelete()
      ()
    }
  }

  class BaseLMDBOpsBenchState {
    implicit val codecC: Codec[Channel]       = serializeChannel.toCodec
    implicit val codecP: Codec[Pattern]       = serializePattern.toCodec
    implicit val codecA: Codec[Entry]         = serializeInfo.toCodec
    implicit val codecK: Codec[EntriesCaptor] = serializeEntriesCaptor.toCodec

    val mapSize: Long  = 1024L * 1024L * 1024L
    val noTls: Boolean = false

    var dbDir: Path              = _
    var dbGnats: Dbi[ByteBuffer] = _
    var dbJoins: Dbi[ByteBuffer] = _
    var env: Env[ByteBuffer]     = _

    val randomData = generate(100, 100)

    def indexedKey(i: Int): ByteBuffer = {
      val bytes = Array.fill[Byte](2)(Random.nextInt().toByte)
      val mbb   = ByteBuffer.allocateDirect(bytes.length + 4)
      mbb.put(bytes)
      mbb.putInt(i)
      println(mbb.getInt(2))
      println(s"!!!(${mbb.get(0)}${mbb.get(1)})==${mbb.getInt(2)} ---- $i")
      mbb.flip()
      mbb
    }

    def populate(): Unit =
      randomData.foreach(x => {
        val txn = env.txnWrite()
        dbGnats.put(txn, x._1, x._2)
        txn.commit()
      })

    def setup(): Unit = {
      dbDir = Files.createTempDirectory("rchain-rspace-mixed-bench-")
      val branch = Branch.MASTER
      val flags  = List(EnvFlags.MDB_NOTLS)
      env = Context.env(dbDir, mapSize, flags)
      dbGnats = env.openDbi(s"${branch.name}-gnats", MDB_CREATE)
      dbJoins = env.openDbi(s"${branch.name}-joins", MDB_CREATE)
      val trieStore = LMDBTrieStore
        .create[Blake2b256Hash, GNAT[Channel, Pattern, Entry, EntriesCaptor]](env, dbDir)
      store = new LMDBStore[Channel, Pattern, Entry, EntriesCaptor](
        env,
        dbDir,
        dbGnats,
        dbJoins,
        trieStore,
        branch
      )
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
      })
      data.map(x => {
        val txn = env.txnRead()
        val r   = (x._1, db.get(txn, x._1))
        txn.commit()
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

    var store: LMDBStore[Channel, Pattern, Entry, EntriesCaptor] = _
  }

}
