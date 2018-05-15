package coop.rchain.rspace.test

import coop.rchain.rspace.Blake2b256Hash
import org.scalacheck.Arbitrary

object ArbitraryInstances {

  implicit val arbitraryBlake2b256Hash: Arbitrary[Blake2b256Hash] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => Blake2b256Hash.create(bytes)))
}
