package coop.rchain.rspace

import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector
import coop.rchain.rspace.util.{memcmp, veccmp}

class UtilTests extends FlatSpec with Matchers {

  def mem(a: ByteVector, b: ByteVector): Int =
    memcmp(a.toArray, b.toArray)

  "veccmp & memcmp" should "work" in {
    val ve    = ByteVector.empty
    val v0    = ByteVector(0)
    val v1    = ByteVector(1)
    val v123  = ByteVector(1, 2, 3)
    val v124  = ByteVector(1, 2, 4)
    val v1234 = ByteVector(1, 2, 3, 4)

    veccmp(ve, ve) shouldBe 0
    mem(ve, ve) shouldBe 0

    veccmp(ve, v0) shouldBe -1
    mem(ve, v0) shouldBe -1

    veccmp(v0, ve) shouldBe 1
    mem(v0, ve) shouldBe 1

    veccmp(v0, v1) shouldBe -1
    mem(v0, v1) shouldBe -1

    veccmp(v1, v0) shouldBe 1
    mem(v1, v0) shouldBe 1

    veccmp(v123, v123) shouldBe 0
    mem(v123, v123) shouldBe 0

    veccmp(v123, v1234) shouldBe -1
    mem(v123, v1234) shouldBe -1

    veccmp(v1234, v123) shouldBe 1
    mem(v1234, v123) shouldBe 1

    veccmp(v123, v124) shouldBe -1
    mem(v123, v124) shouldBe -1

    veccmp(v124, v123) shouldBe 1
    mem(v124, v123) shouldBe 1
  }
}
