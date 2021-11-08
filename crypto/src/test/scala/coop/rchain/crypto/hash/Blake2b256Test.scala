package coop.rchain.crypto.hash

import coop.rchain.crypto.codec._
import coop.rchain.shared.Base16
import org.scalatest.{AppendedClues, BeforeAndAfterEach, FunSpec, Matchers}

class Blake2b256Test extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {
  describe("Blake2b256 hashing algorithm") {
    it("encodes empty") {
      val result = Base16.encode(Blake2b256.hash("".getBytes))
      result shouldBe "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
    }
    it("encodes data") {
      val result = Base16.encode(Blake2b256.hash("abc".getBytes))
      result shouldBe "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
    }
  }
}
