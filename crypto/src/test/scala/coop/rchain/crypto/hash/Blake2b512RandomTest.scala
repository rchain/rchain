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
      "52884e9cfaf738709d271e9c0268f05964395678d9ccd61b187d67224a464230"
    )
    Base16.encode(res2) should be(
      "cfa2ebb91185e9764a5aef6c7f5a6756cfe0f48a33c5bfabffdacac55d32f24a"
    )
  }
  it should "handle splitShort as well." in {
    val b2Random = Blake2b512Random(emptyMsg)
    val split    = b2Random.splitShort(0x6487)
    val res1     = split.next()
    val res2     = split.next()
    Base16.encode(res1) should be(
      "745ce0f59aa7ebadc31c097126ac85870c3364b561d1d81935eb01ef5968d4b3"
    )
    Base16.encode(res2) should be(
      "2d7bd219e4ce1e18e38c06eecdf17098ed49d66890086d19543a84fe88d80d67"
    )
  }
  it should "correctly implement wraparound." in {
    val b2Random = Blake2b512Random(emptyMsg)
    Blake2b512Random.tweakLength0(b2Random)
    val res1 = b2Random.next()
    val res2 = b2Random.next()
    val res3 = b2Random.next()
    val res4 = b2Random.next()
    Base16.encode(res1) should be(
      "b63ea0e23d853977e02707364c753bd414c4828e294c1b0c39d046bacf18f5cf"
    )
    Base16.encode(res2) should be(
      "4b850fc7d0a930cd89a8907ccee22f41941bd896127e71301eba137a347b131f"
    )
    Base16.encode(res3) should be(
      "a913716961120edbbb9f08cc513a40321de334aa99a6991f97eff93b799f9cab"
    )
    Base16.encode(res4) should be(
      "be14efe796e8a10a8bbc55e4691f8eeb71df5dc37b0b0b79133150b8cc90533a"
    )
  }

  it should "roll over when enough byte-splits have occurred" in {
    val b2Random = Blake2b512Random(emptyMsg)
    val rollover = 0.to(112).foldLeft(b2Random) { (rand, n) =>
      rand.splitByte(n.toByte)
    }
    val res1 = rollover.next()
    val res2 = rollover.next()
    Base16.encode(res1) should be(
      "0f6ccee70daf946d23361a92e672515898a287456c38517bd92bb0925ee18103"
    )
    Base16.encode(res2) should be(
      "7d9a1831ce7f42b818c61223f709d55dd9cf310b01e19e2526d2d13e1b9ed61c"
    )
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
      "1fa2af2fdc0521dacc1b06d0dc9ee729075283c7e2ba8df7b637bd05134e2d30"
    )
    Base16.encode(res2) should be(
      "9c7a9b1f04907c73263a81178540a5d7e3102fe260a9f15b80ff91f87de2039b"
    )
  }
  val partialMsg: Array[Byte] = "Hello, World!".getBytes(StandardCharsets.UTF_8)
  "Partial" should "give a predictable result" in {
    val b2Random = Blake2b512Random(partialMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "34c06b6f6907595709c44a1c2f4940210d99b04302937a88e14a5c5e2d439221"
    )
    Base16.encode(res2) should be(
      "7c3c57f0220fa003ad9c10fd785001c11f2b626f0d5da8367499200e10166276"
    )
  }
  val singleBlockMsg: Array[Byte] =
    "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa "
      .getBytes(StandardCharsets.UTF_8)
  "Single block prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(singleBlockMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "459691c149f10c8cf45a4f84421d89e97228b91e046f7afbcf3a4131216c538b"
    )
    Base16.encode(res2) should be(
      "1e676c4ad57a46408312a5209e8498d43023e43ab0bfabfc57a535663dfa3918"
    )
  }
  val blockAndPartMsg: Array[Byte] =
    "quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores "
      .getBytes(StandardCharsets.UTF_8)
  "Single block and additional partial prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(blockAndPartMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "c27d88a63898e9f593ae34439112572feedd241c4223e6c62e997e45267b285d"
    )
    Base16.encode(res2) should be(
      "8c8d36972e4bfa65fdde555c1247ee221ff7c0031e92ec790aa01549321e7c86"
    )
  }
  val multiBlockMsg: Array[Byte] =
    "eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem "
      .getBytes(StandardCharsets.UTF_8)
  "Multi block prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(multiBlockMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "07ac715093bb984b8f9364b6ccdf89ca63dbdcc164000d115ee333d6566b3e87"
    )
    Base16.encode(res2) should be(
      "1235bb1ec4ba9bf58f6f0aa10aaf53e373191a1c6a849fbc7b8a1d31a0affc61"
    )
  }
  val multiBlockAndPartialMsg: Array[Byte] =
    "Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?\nAt vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga."
      .getBytes(StandardCharsets.UTF_8)
  "Multi block and partial prefix" should "give a predictable result" in {
    val b2Random = Blake2b512Random(multiBlockAndPartialMsg)
    val res1     = b2Random.next()
    val res2     = b2Random.next()
    Base16.encode(res1) should be(
      "828f766bc845c41944f1a9933b0835afabf636abd93f8bd986db7a73c7de056c"
    )
    Base16.encode(res2) should be(
      "890399ff51fd6f02c09ad76d69c51445805252c60d7511a767e2e01dce0c45cd"
    )
  }

  "A merge with a single child" should "give a predictable result" in {
    val b2RandomBase      = Blake2b512Random(emptyMsg)
    val b2Random0         = b2RandomBase.splitByte(0)
    val singleMergeRandom = Blake2b512Random.merge(List(b2Random0))
    val res1              = singleMergeRandom.next()
    val res2              = singleMergeRandom.next()
    Base16.encode(res1) should be(
      "a3904853d9db8de44202bf6ab64c2ee5b6c78fe8abc7799dc0e3426d6572e4eb"
    )
    Base16.encode(res2) should be(
      "1d6da76e99ce1fa6fe756f7d7117c3eae6c0fd4fa53854e04de3083d557d0a01"
    )
  }

  "A merge with two children" should "give a predictable result" in {
    val b2RandomBase      = Blake2b512Random(emptyMsg)
    val b2Random0         = b2RandomBase.splitByte(0)
    val b2Random1         = b2RandomBase.splitByte(1)
    val singleMergeRandom = Blake2b512Random.merge(List(b2Random0, b2Random1))
    val res1              = singleMergeRandom.next()
    val res2              = singleMergeRandom.next()
    Base16.encode(res1) should be(
      "ce190f4283d4b11653cb78ee8fbc68a5b8cb62511a1f2ed3e836400e62144fa9"
    )
    Base16.encode(res2) should be(
      "460e913fb6f2250fb1ae2cd6ceeb5501b0d83b29abd538d3508ec6845904342d"
    )
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
      "1af9db74651a8aa5e667311151d6b556939d0eb478980cce2ebd0b2cbc7183a8"
    )
    Base16.encode(res2) should be(
      "e6015287968435ef7b9656daf52083619aeb237db24bf09b2f878575d616572a"
    )
  }
}
