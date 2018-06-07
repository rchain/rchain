package coop.rchain.rspace.history

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test.roundTripCodec
import coop.rchain.shared.AttemptOps._
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

  val fullPb: PointerBlock = PointerBlock.fromVector(Vector.fill(256)(Some(helloHash)))

  "An empty PointerBlock" should "have the expected hash" in {

    val expected = "2b69702a889248a4d6620475a105dccd5e0d4230aca8a492aaf6510e55d55b02"

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

    val pb = emptyPb.updated(List((1, Some(helloHash))))

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(pb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.bytes.toArray).bytes.toHex)

    val expected = "b11665e990d461db27f850481ad538662cc5321d67f9688c3a6ae77fa4b63f03"

    attemptedActual.get shouldBe expected
  }

  "A PointerBlock with a known item at index 42" should "have the expected hash" in {

    val pb = emptyPb.updated(List((42, Some(helloHash))))

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(pb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.bytes.toArray).bytes.toHex)

    val expected = "bc50d1148e6c4197bc978a80e424d4a0b3f065496102d376ba6c138a2ed2c3a7"

    attemptedActual.get shouldBe expected
  }

  "A PointerBlock with a known item at all indices" should "have the expected hash" in {

    val attemptedActual = implicitly[Codec[PointerBlock]]
      .encode(fullPb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.bytes.toArray).bytes.toHex)

    val expected = "2b5e43a142c6b5e1b2ff614185d76d1e215b0efe627e124b4244006f4da4ed64"

    attemptedActual.get shouldBe expected
  }
}
