package coop.rchain.crypto.signatures
import coop.rchain.crypto.hash.Sha256
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.shared.Base16
import org.scalatest.{AppendedClues, BeforeAndAfterEach}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Secp256k1Test extends AnyFunSpec with Matchers with BeforeAndAfterEach with AppendedClues {
  describe("Secp256k1") {

    it("verifies the given secp256k1 signature in native code with keypair") {
      val (PrivateKey(sec), PublicKey(pub)) = Secp256k1.newKeyPair
      val data                              = Sha256.hash("testing".getBytes)
      val sig                               = Secp256k1.sign(data, sec)
      Secp256k1.verify(data, sig, pub) shouldBe true
    }
    it("verifies the given secp256k1 signature in native code") {
      val data = Sha256.hash("testing".getBytes)
      val sig = Base16.unsafeDecode(
        "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589"
      )
      val pub = Base16.unsafeDecode(
        "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40"
      )
      Secp256k1.verify(data, sig, pub)
    }
    it("packaged in libsecp256k1 creates an ECDSA signature") {
      val data = Sha256.hash("testing".getBytes)
      val sec =
        Base16.unsafeDecode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
      Base16
        .encode(Secp256k1.sign(data, sec))
        .toUpperCase() shouldBe "30440220182A108E1448DC8F1FB467D06A0F3BB8EA0533584CB954EF8DA112F1D60E39A202201C66F36DA211C087F3AF88B50EDF4F9BDAA6CF5FD6817E74DCA34DB12390C6E9"
    }
    it("verify returns true if valid, false if invalid") {
      val sec =
        Base16.unsafeDecode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
      Secp256k1.secKeyVerify(sec) shouldBe true
    }
    it("computes public key from secret key") {
      val sec =
        Base16.unsafeDecode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
      Base16
        .encode(Secp256k1.toPublic(sec))
        .toUpperCase() shouldBe "04C591A8FF19AC9C4E4E5793673B83123437E975285E7B442F4EE2654DFFCA5E2D2103ED494718C697AC9AEBCFD19612E224DB46661011863ED2FC54E71861E2A6"
    }
  }

}
