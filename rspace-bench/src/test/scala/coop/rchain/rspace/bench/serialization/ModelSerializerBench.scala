package coop.rchain.rspace.bench.serialization

import java.util.concurrent.TimeUnit
import java.nio.ByteBuffer

import coop.rchain.models._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.Serialize
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import coop.rchain.rspace.bench.serialization._

import org.openjdk.jmh.annotations.{State => BenchState, _}
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OperationsPerInvocation(value = 100)
class ParSerializerBench {

  import coop.rchain.rholang.interpreter.storage.implicits._

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoLight(bh: Blackhole, state: ProtobufModelBenchState) =
    state.lightPars.foreach(e => bh.consume(state.roundTrip(e)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoMid(bh: Blackhole, state: ProtobufModelBenchState) =
    state.midPars.foreach(e => bh.consume(state.roundTrip(e)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoHeavy(bh: Blackhole, state: ProtobufModelBenchState) =
    state.heavyPars.foreach(e => bh.consume(state.roundTrip(e)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoLight(bh: Blackhole, state: KryoModelBenchState) =
    state.lightPars.foreach(e => bh.consume(state.roundTrip(e)(state.parSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoMid(bh: Blackhole, state: KryoModelBenchState) =
    state.midPars.foreach(e => bh.consume(state.roundTrip(e)(state.parSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoHeavy(bh: Blackhole, state: KryoModelBenchState) =
    state.heavyPars.foreach(e => bh.consume(state.roundTrip(e)(state.parSerializer)))

}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OperationsPerInvocation(value = 100)
class ExprSerializerBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoLight(bh: Blackhole, state: ProtobufModelBenchState) =
    state.lightExprs.foreach(e => bh.consume(state.roundTrip(e)(state.serializeExpr)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoMid(bh: Blackhole, state: ProtobufModelBenchState) =
    state.midExprs.foreach(e => bh.consume(state.roundTrip(e)(state.serializeExpr)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoHeavy(bh: Blackhole, state: ProtobufModelBenchState) =
    state.heavyExprs.foreach(e => bh.consume(state.roundTrip(e)(state.serializeExpr)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoLight(bh: Blackhole, state: KryoModelBenchState) =
    state.lightExprs.foreach(e => bh.consume(state.roundTrip(e)(state.exprSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoMid(bh: Blackhole, state: KryoModelBenchState) =
    state.midExprs.foreach(e => bh.consume(state.roundTrip(e)(state.exprSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoHeavy(bh: Blackhole, state: KryoModelBenchState) =
    state.heavyExprs.foreach(e => bh.consume(state.roundTrip(e)(state.exprSerializer)))

}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OperationsPerInvocation(value = 100)
class GnatSerializerBench {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protoLight(bh: Blackhole, state: RspaceProtobufModelBenchState) =
    state.lightGnats.foreach(gnat => bh.consume(state.roundTrip(gnat)(state.gnatSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protobufMid(bh: Blackhole, state: RspaceProtobufModelBenchState) =
    state.midGnats.foreach(gnat => bh.consume(state.roundTrip(gnat)(state.gnatSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def protobufHeavy(bh: Blackhole, state: RspaceProtobufModelBenchState) =
    state.heavyGnats.foreach(gnat => bh.consume(state.roundTrip(gnat)(state.gnatSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoLight(bh: Blackhole, state: KryoModelBenchState) =
    state.lightGnats.foreach(gnat => bh.consume(state.roundTrip(gnat)(state.gnatSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoMid(bh: Blackhole, state: KryoModelBenchState) =
    state.midGnats.foreach(gnat => bh.consume(state.roundTrip(gnat)(state.gnatSerializer)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def kryoHeavy(bh: Blackhole, state: KryoModelBenchState) =
    state.heavyGnats.foreach(gnat => bh.consume(state.roundTrip(gnat)(state.gnatSerializer)))

}

abstract class ModelSerializerBenchState {
  type TestGNAT = GNAT[Par, BindPattern, ListParWithRandom, TaggedContinuation]

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
      val constrainedSize = Math.max(1, size)
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
  val elements = 100

  def generate[A: Arbitrary](size: Int): Seq[A] = {
    val params = Parameters.default.withSize(size)
    (1 to elements).map(
      i => implicitly[Arbitrary[A]].arbitrary.apply(params, Seed(initSeed + i)).get
    )
  }

  val lightGnats = generate[TestGNAT](1)(arbitraryGnat)
  val midGnats   = generate[TestGNAT](5)(arbitraryGnat)
  val heavyGnats = generate[TestGNAT](10)(arbitraryGnat)

  val lightPars = generate[Par](1)
  val midPars   = generate[Par](5)
  val heavyPars = generate[Par](10)

  val lightExprs = generate[Expr](1)
  val midExprs   = generate[Expr](5)
  val heavyExprs = generate[Expr](10)

}

@BenchState(Scope.Benchmark)
class RspaceProtobufModelBenchState extends ModelSerializerBenchState {

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

  val gnatSerializer = new Serialize2ByteBuffer[TestGNAT] {
    override def encode(a: TestGNAT): ByteBuffer =
      cg.encode(a).get.toByteVector.toDirectByteBuffer
    override def decode(bytes: ByteBuffer): TestGNAT =
      cg.decode(BitVector(bytes)).get.value
  }

  def roundTrip[A](in: A)(implicit s: Serialize2ByteBuffer[A]): A = {
    val meta = s.encode(in)
    val out  = s.decode(meta)
    out
  }
}

@BenchState(Scope.Benchmark)
class ProtobufModelBenchState extends ModelSerializerBenchState {

  import coop.rchain.models.serialization.implicits._
  implicit val serializeExpr: Serialize[Expr] =
    mkProtobufInstance(Expr)

  def roundTrip[T: Serialize](in: T): T = {
    val ser = Serialize[T].encode(in)
    val res = Serialize[T].decode(ser)
    res.right.get
  }

}

@BenchState(Scope.Benchmark)
class KryoModelBenchState extends ModelSerializerBenchState {
  import KryoSerializers._
  val gnatSerializer = KryoSerializers.serializer(classOf[TestGNAT])
  val parSerializer  = KryoSerializers.serializer(classOf[Par])
  val exprSerializer = KryoSerializers.serializer(classOf[Expr])

  def roundTrip[A](in: A)(implicit s: Serialize2ByteBuffer[A]): A = {
    val meta = s.encode(in)
    val out  = s.decode(meta)
    out
  }
}
