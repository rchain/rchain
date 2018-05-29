package coop.rchain.rspace.test

import coop.rchain.rspace._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringMatch, Wildcard}
import coop.rchain.rspace.history._
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.ByteVector

object ArbitraryInstances {

  implicit val arbitraryPattern: Arbitrary[Pattern] = {
    val genWildcard: Gen[Pattern] = Gen.const(Wildcard)
    val genMatch: Gen[Pattern]    = Arbitrary.arbitrary[String].map((str: String) => StringMatch(str))
    val genPattern: Gen[Pattern]  = Gen.oneOf(genWildcard, genMatch)
    Arbitrary(genPattern)
  }

  implicit val arbitraryBlake2b256Hash: Arbitrary[Blake2b256Hash] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => Blake2b256Hash.create(bytes)))

  implicit val arbitraryByteVector: Arbitrary[ByteVector] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => ByteVector(bytes)))

  implicit val arbitraryPointerBlock: Arbitrary[PointerBlock] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(256, Arbitrary.arbitrary[Option[Blake2b256Hash]])
        .map(maybeHashes => PointerBlock.fromVector(maybeHashes.toVector))
    })

  implicit def arbitaryTrie[K, V](implicit
                                  arbK: Arbitrary[K],
                                  arbV: Arbitrary[V]): Arbitrary[Trie[K, V]] = {
    val genNode: Gen[Trie[K, V]] = Arbitrary.arbitrary[PointerBlock].map(pb => Node(pb))

    val genLeaf: Gen[Trie[K, V]] =
      for {
        k <- arbK.arbitrary
        v <- arbV.arbitrary
      } yield Leaf(k, v)

    val genTrie: Gen[Trie[K, V]] =
      Gen.oneOf(genLeaf, genNode)

    Arbitrary(genTrie)
  }

  implicit val arbitraryTestKey: Arbitrary[TestKey] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(4, Arbitrary.arbitrary[Int])
        .map(ints => TestKey.create(ints))
    })

  implicit val arbitraryNonEmptyMapTestKeyByteVector: Arbitrary[Map[TestKey, ByteVector]] = {
    Arbitrary(
      Gen
        .sized { size =>
          Gen.containerOfN[Seq, (TestKey, ByteVector)](if (size > 2) size else 2,
                                                       Arbitrary.arbitrary[(TestKey, ByteVector)])
        }
        .map(_.toMap))
  }
}
