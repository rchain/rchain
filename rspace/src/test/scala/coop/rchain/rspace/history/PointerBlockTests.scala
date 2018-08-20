package coop.rchain.rspace.history

import coop.rchain.rspace.internal._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test.roundTripCodec
import org.scalacheck.Prop
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.{Checkers, Configuration}
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.BitVector
import scodec.{Codec, DecodeResult}

class PointerBlockTests extends FlatSpec with Matchers with Checkers with Configuration {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1000))

  "A PointerBlock" should "be the same when round-tripped with scodec" in {

    val propRoundTripCodec: Prop = Prop.forAll { (pb: PointerBlock) =>
      roundTripCodec[PointerBlock](pb)
        .map((value: DecodeResult[PointerBlock]) => value.value == pb)
        .getOrElse(default = false)
    }

    check(propRoundTripCodec)
  }

  val emptyPb: PointerBlock = PointerBlock.create()

  val emptyPbHash: Blake2b256Hash =
    implicitly[Codec[PointerBlock]]
      .encode(emptyPb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
      .get

  val helloHash: Blake2b256Hash = Blake2b256Hash.create("hello".getBytes())

  val fullPb: PointerBlock = PointerBlock.fromVector(Vector.fill(256)(LeafPointer(helloHash)))

  "An empty PointerBlock" should "have the expected hash" in {

    val expected = "27cee3d346eb76d15aa439d1840db71030276e75b495e031c7eb4638a5286312"

    emptyPbHash.bytes.toHex shouldBe expected
  }

  "A serialized empty PointerBlock" should "have the expected length" in {

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(emptyPb)
      .map((vector: BitVector) => vector.bytes.length)

    attemptedActual.get shouldBe PointerBlock.length
  }

  "A serialized full PointerBlock" should "have the expected length" in {

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(fullPb)
      .map((vector: BitVector) => vector.bytes.length)

    attemptedActual.get shouldBe (PointerBlock.length * (Blake2b256Hash.length + 1))
  }

  "A PointerBlock with a known item at index 1" should "have the expected hash" in {

    val pb = emptyPb.updated(List((1, LeafPointer(helloHash))))

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(pb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.bytes.toArray).bytes.toHex)

    val expected = "4074823128c150c0e68d0ecddbd5db1c5d4f75661ebc37461a4badd85a329835"

    attemptedActual.get shouldBe expected
  }

  "A PointerBlock with a known item at index 42" should "have the expected hash" in {

    val pb = emptyPb.updated(List((42, LeafPointer(helloHash))))

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(pb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.bytes.toArray).bytes.toHex)

    val expected = "54c4559c870bcaa6459faf121aec85e43318b9c55bab297652da6fdbbfa669ad"

    attemptedActual.get shouldBe expected
  }

  "A PointerBlock with a known item at all indices" should "have the expected hash" in {

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(fullPb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.bytes.toArray).bytes.toHex)

    val expected = "51893a2c8a876cd99f841acf6cc1ddf429fc8d905d5897ddb9a7a44910e6c538"

    attemptedActual.get shouldBe expected
  }
}
