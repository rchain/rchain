package coop.rchain.rspace.test

import coop.rchain.rspace.{Blake2b256Hash, PointerBlock}
import org.scalacheck.{Arbitrary, Gen}

object ArbitraryInstances {

  implicit val arbitraryBlake2b256Hash: Arbitrary[Blake2b256Hash] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => Blake2b256Hash.create(bytes)))

  implicit val arbitraryPointerBlock: Arbitrary[PointerBlock] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(256, Arbitrary.arbitrary[Option[Blake2b256Hash]])
        .map(maybeHashes => PointerBlock.fromVector(maybeHashes.toVector))
    })
}
