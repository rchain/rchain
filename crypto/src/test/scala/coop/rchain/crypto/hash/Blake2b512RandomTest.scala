package coop.rchain.crypto.hash

import coop.rchain.crypto.codec._
import org.scalatest._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.prop.{Checkers, Configuration}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.Arrays

/**
  * All of the test vectors here were created with the b2sum utility and a
  * hexeditor. For the initialization tests, it as simple as padding to a
  * complete block length, and then adding an additional blank block.
  * For the rollover tests, we use the block format which is 112 bytes of path
  * info, followed by 16 bytes of length info in little endian format.
  */
class Blake2b512RandomSpec extends FlatSpec with Matchers with Checkers with Configuration {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "An arbitrary random-state" should "survive serialization round-tripping" in {
    val propRoundTripTypeMapper: Prop = Prop.forAll { (rand: Blake2b512Random) =>
      val tm          = Blake2b512Random.typeMapper
      val half        = tm.toBase(rand)
      val result      = tm.toCustom(half)
      val onceAndHalf = tm.toBase(result)
      val repeat      = tm.toBase(rand)

      (rand == result || { println("not same"); false }) &&
      (half == repeat || {
        println("not repeatable encoding.")
        println(s"halfEncode:   ${Base16.encode(half.toByteArray)}")
        println(s"repeatEncode: ${Base16.encode(repeat.toByteArray)}")
        false
      }) &&
      (half == onceAndHalf || {
        println("not same encoding.")
        println(s"halfEncode:        ${Base16.encode(half.toByteArray())}")
        println(s"onceAndHalfEncode: ${Base16.encode(onceAndHalf.toByteArray())}")
        false
      }) && {
        val baseNext   = rand.next()
        val resultNext = result.next()
        if (Arrays.equals(baseNext, resultNext))
          true
        else {
          println("not same next.")
          println(s"randEncode:   ${Base16.encode(tm.toBase(rand).toByteArray)}")
          println(s"resultEncode: ${Base16.encode(tm.toBase(result).toByteArray)}")
          println(s"baseNext:   ${Base16.encode(baseNext)}")
          println(s"resultNext: ${Base16.encode(resultNext)}")
          false
        }
      }
    }

    check(propRoundTripTypeMapper)

    val zeroPositionRand = Arbitrary.arbitrary[Blake2b512Random] suchThat (_.getPosition == 0)
    val propNextNotSame: Prop = Prop.forAll(zeroPositionRand) { (rand: Blake2b512Random) =>
      val tm       = Blake2b512Random.typeMapper
      val randCopy = rand.copy()
      randCopy.next()
      tm.toBase(randCopy) != tm.toBase(rand) || {
        println("next1")
        println(s"randEncode: ${Base16.encode(tm.toBase(rand).toByteArray)}")
        println(s"randCopyEncode: ${Base16.encode(tm.toBase(randCopy).toByteArray)}")
        false
      }
      randCopy.next()
      tm.toBase(randCopy) != tm.toBase(rand) || {
        println("next2")
        println(s"randEncode: ${Base16.encode(tm.toBase(rand).toByteArray)}")
        println(s"randCopyEncode: ${Base16.encode(tm.toBase(randCopy).toByteArray)}")
        false
      }
    }

    check(propNextNotSame)

    val propSplitNotSame: Prop = Prop.forAll(zeroPositionRand) { (rand: Blake2b512Random) =>
      val tm       = Blake2b512Random.typeMapper
      val randCopy = rand.splitByte(0)
      tm.toBase(randCopy) != tm.toBase(rand) || {
        println("split")
        println(s"randEncode: ${Base16.encode(tm.toBase(rand).toByteArray)}")
        println(s"randCopyEncode: ${Base16.encode(tm.toBase(randCopy).toByteArray)}")
        false
      }
    }

    check(propSplitNotSame)
  }

  val emptyMsg: Array[Byte] = new Array[Byte](0)
  "Empty" should "give a predictable result" in {
    val b2Random = Blake2b512Random(emptyMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "b962d59c94308015793b1f62f78d473f847dbf275ca199e315a512fa39aaf342")
    Base16.encode(res2) should be(
      "3cc42eba719df4a9f3ef7335ba700892b58e1344f113684a0afda52edd8beed4")
  }
  it should "handle splitShort as well." in {
    val b2Random = Blake2b512Random(emptyMsg)
    val split    = b2Random.splitShort(0x6487)
    val res1     = split.next()
    val res2     = split.next()
    Base16.encode(res1) should be(
      "00baadcd599f38b3307e3e85b9a0cbc2a47588fb77276f6b85af5eb4e441e43c")
    Base16.encode(res2) should be(
      "1b2ddd2f756ef8009b7d65be12f2a2aa92ad17e2bfe57d6643c8f9d39520d53b")
  }
  it should "correctly implement wraparound." in {
    val b2Random = Blake2b512Random(emptyMsg)
    Blake2b512Random.tweakLength0(b2Random)
    val res1 = b2Random.next()
    val res2 = b2Random.next()
    val res3 = b2Random.next()
    val res4 = b2Random.next()
    Base16.encode(res1) should be(
      "317cd07cb364e94ace11cedb496aee1a7985845d37c5c65a22ac3223eb6d6827")
    Base16.encode(res2) should be(
      "ecc35e232fd8fe53c4f28574d3afd0bc8b53483c46b0a4ad3899b1fada492ae4")
    Base16.encode(res3) should be(
      "70a93a3dd599328caf228044299c755aa5e736fa197672a4c21b25d827eef1d7")
    Base16.encode(res4) should be(
      "8b82080409a6841e104f137db737b12a49151fc77ea52c46da54072005c66420")
  }

  it should "roll over when enough byte-splits have occurred" in {
    val b2Random = Blake2b512Random(emptyMsg)
    val rollover = 0.to(112).foldLeft(b2Random) { (rand, n) =>
      rand.splitByte(n.toByte)
    }
    val res1 = rollover.next()
    val res2 = rollover.next()
    Base16.encode(res1) should be(
      "fbcbf93f84ae40261d897d0863ddef870b2abe46a23208b43a0f56a766f8bc7a")
    Base16.encode(res2) should be(
      "e40e2b87786d256237aa06d73b9a9d329c1b0f8e831ee32d93578325652724b8")
  }
  it should "correctly handle nexts that are then rolled over" in {
    val b2Random = Blake2b512Random(emptyMsg)
    b2Random.next()
    val rollover = 0.to(112).foldLeft(b2Random) { (rand, n) =>
      rand.splitByte(n.toByte)
    }
    val res1 = rollover.next()
    val res2 = rollover.next()

    Base16.encode(res1) should be(
      "edcdd590f7da7cbcd5f21cefcb387022d913dbe45666929a2bfd1e7e4fd6e26c")
    Base16.encode(res2) should be(
      "1db118c61b4e4d5561219506f75930b81a3fff52212b663f413dc06aa9e872c6")
  }
  val partialMsg: Array[Byte] = "Hello, World!".getBytes(StandardCharsets.UTF_8)
  "Partial" should "give a predictable result" in {
    val b2Random = Blake2b512Random(partialMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "efa28ddf7a11184a63d51a2f56cde6169ebd475b68041c2cccb7192902ccc47f")
    Base16.encode(res2) should be(
      "52c4ae2d0d5f59d3076c10d912e60483885f3da72c625390557ceb3494a55c0e")
  }
  val singleBlockMsg: Array[Byte] =
    "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa "
      .getBytes(StandardCharsets.UTF_8)
  "Single block prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(singleBlockMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "223c3480276dd962a817f786578621e708476c4c4f40860d493dcd937200088a")
    Base16.encode(res2) should be(
      "f97e902c46d2d906c18af92d359d41dc045c1ff7c23233b394a2fd4a4e16ebba")
  }
  val blockAndPartMsg: Array[Byte] =
    "quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores "
      .getBytes(StandardCharsets.UTF_8)
  "Single block and additional partial prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(blockAndPartMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "fe72d81cd1121cd027f3b3605ef333436a0019884f351a686c5226a2670787d7")
    Base16.encode(res2) should be(
      "53c7de540222cd37203bdbb01a474a599b9f2fec219db74b830f5c173989768c")
  }
  val multiBlockMsg: Array[Byte] =
    "eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem "
      .getBytes(StandardCharsets.UTF_8)
  "Multi block prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(multiBlockMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "4ed762bcba985d2cb8428fdb6c90956c8f7abe2398c502e06f2e57aea4364306")
    Base16.encode(res2) should be(
      "e87a6119b52c1653f936068fb89bc855e7d201cdf77b1109265f30141914bf65")
  }
  val multiBlockAndPartialMsg: Array[Byte] =
    "Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?\nAt vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga."
      .getBytes(StandardCharsets.UTF_8)
  "Multi block and partial prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(multiBlockAndPartialMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "2fd6059adb28c41de9d5fe952e8ca33beb162868c26749732625dc02f17539d8")
    Base16.encode(res2) should be(
      "57db77d6bdfbcc8474b841431084ffd466c2fbec1f3f00471bf828e692928e5a")
  }

  "A merge with a single child" should "give a predictable result" in {
    val b2RandomBase      = Blake2b512Random(emptyMsg)
    val b2Random0         = b2RandomBase.splitByte(0)
    val singleMergeRandom = Blake2b512Random.merge(List(b2Random0))
    val res1              = singleMergeRandom.next()
    val res2              = singleMergeRandom.next()
    Base16.encode(res1) should be(
      "78e5ca85a2e47c8a8d21ad37cdf7d62ead3d2be6f788f2419c1a25fb42d260c1")
    Base16.encode(res2) should be(
      "0bf7e0587d06b59a819692102dec066e82ec304700aff706f3fbc579fe90050c")
  }

  "A merge with two children" should "give a predictable result" in {
    val b2RandomBase      = Blake2b512Random(emptyMsg)
    val b2Random0         = b2RandomBase.splitByte(0)
    val b2Random1         = b2RandomBase.splitByte(1)
    val singleMergeRandom = Blake2b512Random.merge(List(b2Random0, b2Random1))
    val res1              = singleMergeRandom.next()
    val res2              = singleMergeRandom.next()
    Base16.encode(res1) should be(
      "2d72c9df8496d8dd032994c094e1fff44c058c40aa66ac968af713dd3ce8a425")
    Base16.encode(res2) should be(
      "4c0a5ad2c895d1ce10cc60fff89e73fd1de219ccef44ececeb8ef719ac6a671e")
  }

  "A merge with many children" should "group by 255's until a single internal node is reached." in {
    val builder      = Vector.newBuilder[Blake2b512Random]
    val b2RandomBase = Blake2b512Random(emptyMsg)
    for (i <- 0 until 20) {
      val splitOnce = b2RandomBase.splitByte(i.toByte)
      for (j <- 0 until 255) {
        val splitTwice = splitOnce.splitByte(j.toByte)
        for (k <- 0 until 255) {
          val splitThrice = splitTwice.splitByte(k.toByte)
          builder += splitThrice;
        }
      }
    }
    val merged = Blake2b512Random.merge(builder.result)
    val res1   = merged.next()
    val res2   = merged.next()
    Base16.encode(res1) should be(
      "314730aa276f40180a4dcf69fc35654316770aae63c41a3dd319137359e84480")
    Base16.encode(res2) should be(
      "454fe454f563f729d639d1f542d3744cc3dbc664765548fe03c7c81353ac4aed")
  }
}
