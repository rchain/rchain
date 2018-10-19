package coop.rchain.shared
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import org.scalacheck.Gen

import scala.util.Random

class CompressionSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  val byteArrays =
    for (n <- Gen.choose(10, 500000))
      yield Array.fill(n)((Random.nextInt(256) - 128).toByte)

  describe("Compression") {

    it("should compress without exceptions") {
      forAll(byteArrays) { ar: Array[Byte] =>
        noException should be thrownBy Compression.compress(ar)
      }
    }

    it("decompress should return None when decompresssedLength is incorrect ") {
      forAll(byteArrays) { ar: Array[Byte] =>
        val compressed    = Compression.compress(ar)
        val illegalLength = ar.length + 1
        noException should be thrownBy Compression.decompress(compressed, illegalLength)
        Compression.decompress(compressed, illegalLength) shouldBe None
      }
    }

    it("should decompress to uncompressed data") {
      forAll(byteArrays) { ar: Array[Byte] =>
        val compressed = Compression.compress(ar)
        val backAgain  = Compression.decompress(compressed, ar.length).get
        backAgain.size shouldBe (ar.size)
        backAgain shouldBe ar
      }
    }

    it("should compress effectively when data is compressable (repeatable patterns)") {
      val word       = Array.fill(1024)((Random.nextInt(24) + 33).toByte)
      val ar         = Array.concat(List.fill(1024)(word): _*)
      val compressed = Compression.compress(ar)
      val ratio      = compressed.length.toDouble / ar.length.toDouble
      ar.length should be > compressed.length
      ratio shouldBe (0.005 +- 0.0001)
    }
  }

}
