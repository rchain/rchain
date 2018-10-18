package coop.rchain.rspace.bench.serialization

import java.util.concurrent.TimeUnit
import java.nio.ByteBuffer

import coop.rchain.models.Serialize2ByteBuffer
import coop.rchain.rspace.internal._
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.bench.serialization._

import org.openjdk.jmh.annotations.{State => BenchState, _}
import org.openjdk.jmh.infra.Blackhole

class ExamplesSerializerBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def defaultRoundTrip(bh: Blackhole, state: DefaultExamplesBenchState) = {
    val res = state.roundTrip(state.gnat)(state.serializer)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def defaultHeavyRoundTrip(bh: Blackhole, state: DefaultExamplesBenchState) = {
    val res = state.roundTripMany(state.heavyGnats)(state.serializer)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def kryoRoundTrip(bh: Blackhole, state: KryoExamplesBenchState) = {
    val gnat = state.gnat
    val res  = state.roundTrip(gnat)(state.serializer)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def kryoHeavyRoundTrip(bh: Blackhole, state: KryoExamplesBenchState) = {
    val res = state.roundTripMany(state.heavyGnats)(state.serializer)
    bh.consume(res)
  }
}

abstract class ExamplesSerializerBenchState {
  type TestGNAT = GNAT[Channel, Pattern, Entry, EntriesCaptor]

  implicit def serializer: Serialize2ByteBuffer[TestGNAT]

  def roundTrip[A](e: A)(implicit s: Serialize2ByteBuffer[A]): A = {
    val d   = s.encode(e)
    val res = s.decode(d)
    assert(e == res)
    res
  }

  def roundTripMany[A](seq: Seq[A])(implicit s: Serialize2ByteBuffer[A]): Seq[A] =
    seq.map(roundTrip(_))

  val data = Entry(
    name = Name("Ben", "Serializerovsky"),
    address = Address("1000 Main St", "Crystal Lake", "Idaho", "223322"),
    email = "blablah@tenex.net",
    phone = "555-6969"
  )

  import collection.immutable.Seq

  val channel      = Channel("colleagues")
  val channels     = List(channel, Channel("friends"))
  val datum        = Datum.create(channel, data, false)
  val patterns     = Seq[Pattern](CityMatch(city = "Crystal Lake"))
  val continuation = WaitingContinuation.create(channels, patterns, new EntriesCaptor(), false)

  def gnat() = GNAT[Channel, Pattern, Entry, EntriesCaptor](
    channels,
    Seq(datum),
    Seq(continuation)
  )

  def heavyGnat(weight: Int) = {
    val range                  = (1 to weight)
    val channels: Seq[Channel] = range.map(i => Channel(i.toString))
    val patterns: Seq[Pattern] = range.map(i => CityMatch(city = i.toString))

    GNAT[Channel, Pattern, Entry, EntriesCaptor](
      channels,
      range.map(i => Datum.create(channels(i - 1), data, false)),
      range.map(
        i => WaitingContinuation.create(channels.take(i), patterns, new EntriesCaptor(), false)
      )
    )
  }

  val heavyGnats = (1 to 100).map(i => heavyGnat(i))
}

@BenchState(Scope.Benchmark)
class DefaultExamplesBenchState extends ExamplesSerializerBenchState {

  import scodec.Codec
  import scodec.bits.BitVector

  implicit val cg: Codec[GNAT[Channel, Pattern, Entry, EntriesCaptor]] = codecGNAT(
    implicits.serializeChannel.toCodec,
    implicits.serializePattern.toCodec,
    implicits.serializeInfo.toCodec,
    implicits.serializeEntriesCaptor.toCodec
  )
  import coop.rchain.shared.ByteVectorOps._

  implicit def serializer = new Serialize2ByteBuffer[TestGNAT] {

    override def encode(a: TestGNAT): ByteBuffer =
      cg.encode(a).get.toByteVector.toDirectByteBuffer
    override def decode(bytes: ByteBuffer): TestGNAT =
      cg.decode(BitVector(bytes)).get.value
  }
}

@BenchState(Scope.Benchmark)
class KryoExamplesBenchState extends ExamplesSerializerBenchState {

  import com.esotericsoftware.kryo.Kryo
  import com.esotericsoftware.kryo.io._

  import com.esotericsoftware.kryo.util.MapReferenceResolver
  import org.objenesis.strategy.StdInstantiatorStrategy

  val kryo = new Kryo()
  kryo.setRegistrationRequired(false)
  // Support deserialization of classes without no-arg constructors
  kryo.setInstantiatorStrategy(new StdInstantiatorStrategy())

  implicit def serializer = new Serialize2ByteBuffer[TestGNAT] {

    override def encode(gnat: TestGNAT): ByteBuffer = {
      val output = new ByteBufferOutput(1024, -1)
      kryo.writeObject(output, gnat)
      output.close()

      val buf = output.getByteBuffer
      buf.flip()
      buf
    }

    override def decode(bytes: ByteBuffer): TestGNAT = {
      val input = new ByteBufferInput(bytes)
      val res   = kryo.readObject(input, classOf[TestGNAT])
      input.close()
      res
    }

  }
}
