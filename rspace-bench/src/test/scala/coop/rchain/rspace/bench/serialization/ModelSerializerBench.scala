package coop.rchain.rspace.bench.serialization

import java.util.concurrent.TimeUnit
import java.nio.ByteBuffer

import coop.rchain.models._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.internal._
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import coop.rchain.rspace.bench.serialization._

import org.openjdk.jmh.annotations.{State => BenchState, _}
import org.openjdk.jmh.infra.Blackhole

class ModelSerializerBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def protobufRoundTrip(bh: Blackhole, state: ProtobufModelBenchState) = {
    val res = state.serializer.roundTrip(state.gnat)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 20)
  @Measurement(iterations = 10)
  def protobufHeavyRoundTrip(bh: Blackhole, state: ProtobufModelBenchState) = {
    val res = state.roundTripMany(state.heavyGnats)(state.serializer)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 5)
  @Measurement(iterations = 10)
  def kryoRoundTrip(bh: Blackhole, state: KryoModelBenchState) = {
    val res = state.serializer.roundTrip(state.gnat)
    bh.consume(res)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Fork(value = 1)
  @Warmup(iterations = 20)
  @Measurement(iterations = 10)
  def kryoHeavyRoundTrip(bh: Blackhole, state: KryoModelBenchState) = {
    val res = state.roundTripMany(state.heavyGnats)(state.serializer)
    bh.consume(res)
  }

}

abstract class ModelSerializerBenchState {
  type TestGNAT = GNAT[Par, BindPattern, ListParWithRandom, TaggedContinuation]

  implicit def serializer: Serialize2ByteBuffer[TestGNAT]

  def roundTripMany[A](seq: Seq[A])(implicit s: Serialize2ByteBuffer[A]): Seq[A] =
    seq.map(s.roundTrip(_))

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
        wks <- Gen
                .nonEmptyContainerOf[List, WaitingContinuation[BindPattern, TaggedContinuation]](
                  arbitraryWaitingContinuation[Par, BindPattern, TaggedContinuation](chans).arbitrary
                )
      } yield GNAT(chans, data, wks)
    })

  import coop.rchain.rholang.interpreter.storage.implicits._

  val initSeed = 123456789L

  val gnat: TestGNAT = gnat(10)

  def gnat(size: Int): TestGNAT =
    arbitraryGnat.arbitrary.apply(Parameters.default.withSize(size), Seed(initSeed + size)).get

  val heavyGnats = (1 to 10).map(i => gnat(i))
}

@BenchState(Scope.Benchmark)
class ProtobufModelBenchState extends ModelSerializerBenchState {

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
class KryoModelBenchState extends ModelSerializerBenchState {
  override def serializer = KryoSerializers.serializer(classOf[TestGNAT])
}
