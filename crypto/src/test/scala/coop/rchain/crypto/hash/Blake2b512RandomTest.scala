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

      (Blake2b512Random.same(rand, result) || { println("not same"); false }) &&
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

    val zeroPositionRand = Arbitrary.arbitrary[Blake2b512Random] suchThat (_.position == 0)
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
      "865939e120e6805438478841afb739ae4250cf372653078a065cdcfffca4caf7")
    Base16.encode(res2) should be(
      "98e6d462b65d658fc165782640eded70963449ae1500fb0f24981d7727e22c41")
  }
  it should "handle splitShort as well." in {
    val b2Random = Blake2b512Random(emptyMsg)
    val split    = b2Random.splitShort(0x6487)
    val res1     = split.next()
    val res2     = split.next()
    Base16.encode(res1) should be(
      "7e2e64344f28ecd96af618870b022ecad82db70bf93f3a0cba58374ccecdf6f2")
    Base16.encode(res2) should be(
      "dcb6324399ae0df866bc1a55146dbfee1a3f4ef0ef636612c29cfe7e5f589276")
  }
  it should "correctly implement wraparound." in {
    val b2Random = Blake2b512Random(emptyMsg)
    Blake2b512Random.tweakLength0(b2Random)
    val res1 = b2Random.next()
    val res2 = b2Random.next()
    val res3 = b2Random.next()
    val res4 = b2Random.next()
    Base16.encode(res1) should be(
      "80525d10d1d12e2e4804c39c5de4379c036bb1b3e4b36421f866299dc8f96cb0")
    Base16.encode(res2) should be(
      "631dc986634f04151b4331ed7cf23ad9c781ffdd0310561c119af0e8f6a6ce13")
    Base16.encode(res3) should be(
      "2763f3063ea30905122d34ef1f1cdf03ced8a217b1eabb475568563c932289e6")
    Base16.encode(res4) should be(
      "34326e556fc277872cb17852a31ad78c50ebfbc29913c71a6ef45a0d0c652493")
  }

  it should "roll over when enough byte-splits have occurred" in {
    val b2Random = Blake2b512Random(emptyMsg)
    val rollover = 0.to(112).foldLeft(b2Random) { (rand, n) =>
      rand.splitByte(n.toByte)
    }
    val res1 = rollover.next()
    val res2 = rollover.next()
    Base16.encode(res1) should be(
      "268bffba5e0b42f4102e39b5fc8db69f60d9af43f832d9799bb515bd7ff74bdf")
    Base16.encode(res2) should be(
      "b5c25ea9dcf50d6f71526a573e0b134e9b835e3821923f3ecaa5bf1d31cac912")
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
      "0c6f966e2c1e1dc05173be1a94f0cb1c050015d670ac560f6f156baae3ce2bf4")
    Base16.encode(res2) should be(
      "438ad6889699497717dd4b05864b0e110888ce906b69944d2d9ca426c4baf9b2")
  }
  val partialMsg: Array[Byte] = "Hello, World!".getBytes(StandardCharsets.UTF_8)
  "Partial" should "give a predictable result" in {
    val b2Random = Blake2b512Random(partialMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "aad51609282445d6552486de7709d9e3abbebe3f521762e1cadb1f5c77fd2ca2")
    Base16.encode(res2) should be(
      "de2ea404232f57132cb994fb468f9f3ebf727bab0344f10a54b7a512ab14ba01")
  }
  val singleBlockMsg: Array[Byte] =
    "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa "
      .getBytes(StandardCharsets.UTF_8)
  "Single block prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(singleBlockMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "56f3f54bb2fb5064a69db524618535adbc0d69961c3f7f3a21ed172830d10501")
    Base16.encode(res2) should be(
      "47f76a84573acfb131f0ef2d20623bd43765ff271a0b2ec24452546679375e00")
  }
  val blockAndPartMsg: Array[Byte] =
    "quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores "
      .getBytes(StandardCharsets.UTF_8)
  "Single block and additional partial prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(blockAndPartMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "ba32abd701fe78dd81400ed84b3e6d7e35a5860db79aaa3a8b2f015693bd325f")
    Base16.encode(res2) should be(
      "538b649713f6aa8b57971d344b48f27b84058191194fed0dab56741afaae7b74")
  }
  val multiBlockMsg: Array[Byte] =
    "eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem "
      .getBytes(StandardCharsets.UTF_8)
  "Multi block prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(multiBlockMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "c6b40c41cb9e3c078936c38bddccce3a017de1e4e751799710c9b50a70feb703")
    Base16.encode(res2) should be(
      "b6756c6ebe5e1b2f05ab2212e1a7e8aa126db5637a0111d75715267986df6a95")
  }
  val multiBlockAndPartialMsg: Array[Byte] =
    "Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?\nAt vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga."
      .getBytes(StandardCharsets.UTF_8)
  "Multi block and partial prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(multiBlockAndPartialMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "faa587800d8e45ad49c5c1e4466b7448ec25a96c21906ed74b7d688b7ebfcfe2")
    Base16.encode(res2) should be(
      "9616241d61365f746c257e187ba5f14d33d238bf003d5b4826a79178a6e570c1")
  }
}
