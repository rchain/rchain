package coop.rchain.rholang.crypto

import coop.rchain.crypto.codec._
import org.scalatest._

import java.nio.ByteBuffer

class Blake2b512RandomSpec extends FlatSpec with Matchers {
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
    val b2Random           = Blake2b512Random(emptyMsg)
    val result: ByteBuffer = ByteBuffer.allocate(128)
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
}
