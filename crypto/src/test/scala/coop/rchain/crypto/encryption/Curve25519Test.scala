package coop.rchain.crypto.encryption

import coop.rchain.crypto.codec._
import coop.rchain.shared.Base16
import org.scalatest.{AppendedClues, BeforeAndAfterEach}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Curve25519Test extends AnyFunSpec with Matchers with BeforeAndAfterEach with AppendedClues {
  describe("Curve25519 elliptic curve cryptography") {

    val bobSec =
      Base16.unsafeDecode("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    val bobPub =
      Base16.unsafeDecode("de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")
    val aliceSec =
      Base16.unsafeDecode("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    val alicePub =
      Base16.unsafeDecode("8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")
    val nonce = Base16.unsafeDecode("69696ee955b62b73cd62bda875fc73d68219e0036b7a0b37")
    val message = Base16.unsafeDecode(
      "be075fc53c81f2d5cf141316ebeb0c7b5228c52a4c62cbd44b66849b64244ffce5ecbaaf33bd751a1ac728d45e6c61296cdc3c01233561f41db66cce314adb310e3be8250c46f06dceea3a7fa1348057e2f6556ad6b1318a024a838f21af1fde048977eb48f59ffd4924ca1c60902e52f0a089bc76897040e082f937763848645e0705"
    )
    val cipher = Curve25519.encrypt(alicePub, bobSec, nonce, message)

    it("encodes a cipher") {
      val result = Base16.encode(cipher)
      result shouldBe "f3ffc7703f9400e52a7dfb4b3d3305d98e993b9f48681273c29650ba32fc76ce48332ea7164d96a4476fb8c531a1186ac0dfc17c98dce87b4da7f011ec48c97271d2c20f9b928fe2270d6fb863d51738b48eeee314a7cc8ab932164548e526ae90224368517acfeabd6bb3732bc0e9da99832b61ca01b6de56244a9e88d5f9b37973f622a43d14a6599b1f654cb45a74e355a5"
    }

    it("decrypts") {
      val result = Curve25519.decrypt(bobPub, aliceSec, nonce, cipher).toIndexedSeq
      result shouldBe message.toIndexedSeq
    }

    it("gets public") {
      val result = Curve25519.toPublic(aliceSec).toIndexedSeq
      result shouldBe alicePub.toIndexedSeq
    }
  }
}
