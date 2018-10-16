package coop.rchain.rspace.bench.serialization

import java.util.concurrent.TimeUnit
import java.nio.ByteBuffer

import coop.rchain.models._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.internal._
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

import org.openjdk.jmh.annotations.{State => BenchState, _}
import org.openjdk.jmh.infra.Blackhole

class SerializerBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def protobufRoundTrip(bh: Blackhole, state: ProtobufBenchState) = {
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
  def protobufHeavyRoundTrip(bh: Blackhole, state: ProtobufBenchState) = {
    val res = state.roundTripMany(state.heavyGnats)(state.serializer)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def kryoRoundTrip(bh: Blackhole, state: KryoBenchState) = {
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
  def kryoHeavyRoundTrip(bh: Blackhole, state: KryoBenchState) = {
    val res = state.roundTripMany(state.heavyGnats)(state.serializer)
    bh.consume(res)
  }
}

trait Serialize2ByteBuffer[A] {
  def encode(a: A): ByteBuffer
  def decode(bytes: ByteBuffer): A
}

abstract class SerializerBenchState {
  type TestGNAT = GNAT[Par, BindPattern, ListParWithRandom, TaggedContinuation]

  implicit def serializer: Serialize2ByteBuffer[TestGNAT]

  def roundTrip[A](in: A)(implicit s: Serialize2ByteBuffer[A]): A = {
    val meta = s.encode(in)
    val out  = s.decode(meta)
    assert(out == in)
    out
  }

  def roundTripMany[A](seq: Seq[A])(implicit s: Serialize2ByteBuffer[A]): Seq[A] =
    seq.map(roundTrip(_))

  import collection.immutable.Seq

  import org.scalacheck._
  import org.scalacheck.Arbitrary._

  import coop.rchain.rspace.Serialize

  implicit def arbitraryDatum[C, T](chan: C)(
      implicit
      arbT: Arbitrary[T],
      serializeC: Serialize[C],
      serializeT: Serialize[T]
  ): Arbitrary[Datum[T]] =
    Arbitrary(for {
      t <- arbT.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield Datum.create(chan, t, b))

  def arbitraryWaitingContinuation[C, P, K](chans: List[C])(
      implicit
      arbP: Arbitrary[P],
      arbK: Arbitrary[K],
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): Arbitrary[WaitingContinuation[P, K]] =
    Arbitrary(
      for {
        pats         <- Gen.containerOfN[List, P](chans.length, arbP.arbitrary)
        continuation <- arbK.arbitrary
        boolean      <- Arbitrary.arbitrary[Boolean]
      } yield WaitingContinuation.create(chans, pats, continuation, boolean)
    )

  implicit def arbitraryGnat()(
      implicit
      serializeC: Serialize[Par],
      serializeP: Serialize[BindPattern],
      serializeA: Serialize[ListParWithRandom],
      serializeK: Serialize[TaggedContinuation]
  ): Arbitrary[TestGNAT] =
    Arbitrary(Gen.sized { size =>
      val constrainedSize = if (size > 1) size else 1
      for {
        chans <- Gen.containerOfN[List, Par](constrainedSize, Arbitrary.arbitrary[Par])
        data <- Gen.nonEmptyContainerOf[List, Datum[ListParWithRandom]](
                 arbitraryDatum[Par, ListParWithRandom](chans.head).arbitrary
               )
        wks <- Gen.nonEmptyContainerOf[List, WaitingContinuation[BindPattern, TaggedContinuation]](
                arbitraryWaitingContinuation[Par, BindPattern, TaggedContinuation](chans).arbitrary
              )
      } yield GNAT(chans, data, wks)
    })

  import coop.rchain.rholang.interpreter.storage.implicits._

  val seed = Seed(123456780L)

  def gnat(): TestGNAT =
    arbitraryGnat.arbitrary.apply(Parameters.default.withSize(10), seed).get

  def gnat(weight: Int): TestGNAT =
    arbitraryGnat.arbitrary.apply(Parameters.default.withSize(weight), Seed(123456780L)).get

  val heavyGnats = (1 to 10).map(i => gnat(i))
}

@BenchState(Scope.Benchmark)
class ProtobufBenchState extends SerializerBenchState {

  import scodec.Codec
  import scodec.bits.BitVector

  import coop.rchain.rholang.interpreter.storage.implicits._
  implicit val cg: Codec[TestGNAT] = codecGNAT(
    serializePar.toCodec,
    serializeBindPattern.toCodec,
    serializePars.toCodec,
    serializeTaggedContinuation.toCodec
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
class KryoBenchState extends SerializerBenchState {

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
