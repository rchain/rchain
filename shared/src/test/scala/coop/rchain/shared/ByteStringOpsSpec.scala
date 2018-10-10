package coop.rchain.shared
import com.google.protobuf.ByteString
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import coop.rchain.shared.ByteStringOps._
import org.scalacheck.Gen

import scala.util.Random

class ByteStringOpsSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  val byteStrings =
    for (n <- Gen.choose(1, 500000))
      yield ByteString.copyFrom(Array.fill(n)((Random.nextInt(256) - 128).toByte))

  describe("ByteString") {
    it("should compress without exceptions") {
      forAll(byteStrings) { bs: ByteString =>
        noException should be thrownBy bs.compress
      }
    }

    it("should decompress to uncompressed data") {
      forAll(byteStrings) { bs: ByteString =>
        bs.compress.decompress.get shouldBe bs
      }
    }

    it("decompression should return None when run on uncompressed ByteString") {
      forAll(byteStrings) { bs: ByteString =>
        bs.decompress shouldBe None
      }
    }
  }
}
