package coop.rchain.shared
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import org.scalacheck.Gen

import scala.util.Random

class CompressionSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  val byteArrays =
    for (n <- Gen.choose(1, 500000))
      yield Array.fill(n)((Random.nextInt(256) - 128).toByte)

  describe("Compression") {
    it("should compress without exceptions") {
      forAll(byteArrays) { ar: Array[Byte] =>
        noException should be thrownBy Compression.compress(ar)
      }
    }

    it("should decompress to uncompressed data") {
      forAll(byteArrays) { ar: Array[Byte] =>
        val compressed = Compression.compress(ar)
        val backAgain  = Compression.decompress(compressed, ar.length)
        backAgain.size shouldBe (ar.size)
        backAgain shouldBe ar
      }
    }
  }
}
