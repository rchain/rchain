package coop.rchain.rspace.history

import java.util.Arrays
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test.roundTripCodec
import org.scalacheck.Prop
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import scodec.DecodeResult

class Blake2b256HashTests extends FlatSpec with Checkers {

  "The bytes of a Blake2b256 hash" should "be the same as if it was created directly" in {

    //noinspection ReferenceMustBePrefixed
    val propCreate: Prop = Prop.forAll { (bytes: Array[Byte]) =>
      Arrays.equals(Blake2b256.hash(bytes), Blake2b256Hash.create(bytes).bytes.toArray)
    }

    check(propCreate)
  }

  "A Blake2b256 hash" should "be the same when round-tripped with scodec" in {

    val propRoundTripCodec: Prop = Prop.forAll { (hash: Blake2b256Hash) =>
      roundTripCodec[Blake2b256Hash](hash)
        .map((value: DecodeResult[Blake2b256Hash]) => value.value == hash)
        .getOrElse(default = false)
    }

    check(propRoundTripCodec)
  }
}
