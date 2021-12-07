package coop.rchain.crypto.hash
import coop.rchain.shared.Base16
import org.scalatest.{AppendedClues, BeforeAndAfterEach, FunSpec, Matchers}

class Sha256Test extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {
  describe("Sha256 hashing algorithm") {
    it("encodes") {
      Base16.encode(Sha256.hash("".getBytes)) shouldBe "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      Base16.encode(Sha256.hash("abc".getBytes)) shouldBe "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      Base16.encode(Sha256.hash("hello world".getBytes)) shouldBe "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
      Base16.encode(
        Sha256.hash("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes)
      ) shouldBe "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    }
  }
}
