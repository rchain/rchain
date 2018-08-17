package coop.rchain.rspace.history

import coop.rchain.rspace.internal._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.history.Trie.codecTrie
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test.roundTripCodec
import org.scalacheck.Prop
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.{Checkers, Configuration}
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Codec, DecodeResult}

class TrieTests extends FlatSpec with Matchers with Checkers with Configuration {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1000))

  implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  val emptyTrie: Trie[Nothing, Nothing] = Trie.create()

  "An empty Trie" should "have the expected hash" in {

    val emptyTrieHash =
      implicitly[Codec[Trie[Blake2b256Hash, ByteVector]]]
        .encode(emptyTrie)
        .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
        .get

    val expected = "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3"

    emptyTrieHash.bytes.toHex shouldBe expected
  }

  "A simple leaf" should "have the expected hash" in {

    val helloBytes: Array[Byte]                     = "hello".getBytes
    val helloBytesHash: Blake2b256Hash              = Blake2b256Hash.create(helloBytes)
    val helloLeaf: Leaf[Blake2b256Hash, ByteVector] = Leaf(helloBytesHash, ByteVector(helloBytes))

    val helloLeafHash =
      implicitly[Codec[Trie[Blake2b256Hash, ByteVector]]]
        .encode(helloLeaf)
        .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
        .get

    val expected = "dbad1a97cd55325a85072099ab0ad79ce4c2d1e2d0548a140bd2ec5741a33587"

    helloLeafHash.bytes.toHex shouldBe expected
  }

  "A Trie" should "be the same when round-tripped with scodec" in {

    val propRoundTripCodec: Prop = Prop.forAll { (trie: Trie[Blake2b256Hash, ByteVector]) =>
      roundTripCodec[Trie[Blake2b256Hash, ByteVector]](trie)
        .map((value: DecodeResult[Trie[Blake2b256Hash, ByteVector]]) => value.value == trie)
        .getOrElse(default = false)
    }

    check(propRoundTripCodec)
  }
}
