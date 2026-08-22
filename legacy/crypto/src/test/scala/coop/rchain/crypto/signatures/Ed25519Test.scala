package coop.rchain.crypto.signatures
import coop.rchain.shared.Base16
import org.scalatest.{AppendedClues, BeforeAndAfterEach}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Ed25519Test extends AnyFunSpec with Matchers with BeforeAndAfterEach with AppendedClues {
  describe("Ed25519") {

    it("computes public key from secret key") {
      val sec =
        Base16.unsafeDecode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
      Base16.encode(Ed25519.toPublic(sec)) shouldBe "77f48b59caeda77751ed138b0ec667ff50f8768c25d48309a8f386a2bad187fb"

    }
    it("verifies the given Ed25519 signature") {
      val data = Base16.unsafeDecode(
        "916c7d1d268fc0e77c1bef238432573c39be577bbea0998936add2b50a653171ce18a542b0b7f96c1691a3be6031522894a8634183eda38798a0c5d5d79fbd01dd04a8646d71873b77b221998a81922d8105f892316369d5224c9983372d2313c6b1f4556ea26ba49d46e8b561e0fc76633ac9766e68e21fba7edca93c4c7460376d7f3ac22ff372c18f613f2ae2e856af40"
      )
      val sig = Base16.unsafeDecode(
        "6bd710a368c1249923fc7a1610747403040f0cc30815a00f9ff548a896bbda0b4eb2ca19ebcf917f0f34200a9edbad3901b64ab09cc5ef7b9bcc3c40c0ff7509"
      )
      val pub =
        Base16.unsafeDecode("77f48b59caeda77751ed138b0ec667ff50f8768c25d48309a8f386a2bad187fb")
      Ed25519.verify(data, sig, pub) shouldBe true
    }
    it("creates an Ed25519 signature.") {
      val data = Base16.unsafeDecode(
        "916c7d1d268fc0e77c1bef238432573c39be577bbea0998936add2b50a653171ce18a542b0b7f96c1691a3be6031522894a8634183eda38798a0c5d5d79fbd01dd04a8646d71873b77b221998a81922d8105f892316369d5224c9983372d2313c6b1f4556ea26ba49d46e8b561e0fc76633ac9766e68e21fba7edca93c4c7460376d7f3ac22ff372c18f613f2ae2e856af40"
      )
      val sig = Base16.unsafeDecode(
        "6bd710a368c1249923fc7a1610747403040f0cc30815a00f9ff548a896bbda0b4eb2ca19ebcf917f0f34200a9edbad3901b64ab09cc5ef7b9bcc3c40c0ff7509"
      )
      val sec =
        Base16.unsafeDecode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
      Ed25519.sign(data, sec).deep shouldBe sig.deep
    }
  }
}
